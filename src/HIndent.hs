{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PatternGuards       #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Haskell indenter.
module HIndent
  ( reformat
  , prettyPrint
  , defaultExtensions
  , getExtensions
  , testAst
  ) where

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.ByteString              (ByteString)
import qualified Data.ByteString              as S
import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString.Builder      as S
import qualified Data.ByteString.Char8        as S8
import qualified Data.ByteString.Internal     as S
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Char8   as L8
import qualified Data.ByteString.Unsafe       as S
import qualified Data.ByteString.UTF8         as UTF8
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.Functor.Identity
import           Data.List                    hiding (stripPrefix)
import           Data.Maybe
import           Data.Monoid
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified GHC.Data.EnumSet             as ES
import           GHC.Data.FastString
import           GHC.Data.StringBuffer
import           GHC.Hs
import qualified GHC.LanguageExtensions       as GLP
import qualified GHC.Parser                   as GLP
import           GHC.Parser.Lexer             hiding (buffer)
import           GHC.Types.SrcLoc
import           HIndent.CodeBlock
import qualified HIndent.Extension.Conversion as CE
import           HIndent.ModulePreprocessing
import           HIndent.Pretty
import           HIndent.Types
import qualified Language.Haskell.Extension   as Cabal
import           Prelude
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import           GHC.Utils.Error
import           GHC.Utils.Outputable         hiding (text, (<>))
#endif
-- | Format the given source.
reformat ::
     Config
  -> Maybe [Cabal.Extension]
  -> Maybe FilePath
  -> ByteString
  -> Either String Builder
reformat config mexts mfilepath =
  preserveTrailingNewline
    (fmap (mconcat . intersperse "\n") . mapM processBlock . cppSplitBlocks)
  where
    processBlock :: CodeBlock -> Either String Builder
    processBlock (Shebang text) = Right $ S.byteString text
    processBlock (CPPDirectives text) = Right $ S.byteString text
    processBlock (HaskellSource _ text) =
      let ls = S8.lines text
          prefix = findPrefix ls
          code = unlines' (map (stripPrefix prefix) ls)
          allExts = fromMaybe allExtensions mexts ++ configExtensions config
          opts = parserOptsFromExtensions $ CE.uniqueExtensions allExts
       in case parseModule mfilepath opts (UTF8.toString code) of
            POk _ m ->
              Right $
              S.lazyByteString $
              addPrefix prefix $ S.toLazyByteString $ prettyPrint config m
            PFailed st ->
              Left $
              "Parse failed near " ++
              show ((,) <$> srcLocLine <*> srcLocCol $ psRealLoc $ loc st)
    unlines' = S.concat . intersperse "\n"
    unlines'' = L.concat . intersperse "\n"
    addPrefix :: ByteString -> L8.ByteString -> L8.ByteString
    addPrefix prefix = unlines'' . map (L8.fromStrict prefix <>) . L8.lines
    stripPrefix :: ByteString -> ByteString -> ByteString
    stripPrefix prefix line =
      if S.null (S8.dropWhile (== '\n') line)
        then line
        else fromMaybe (error "Missing expected prefix") . s8_stripPrefix prefix $
             line
    findPrefix :: [ByteString] -> ByteString
    findPrefix = takePrefix False . findSmallestPrefix . dropNewlines
    dropNewlines :: [ByteString] -> [ByteString]
    dropNewlines = filter (not . S.null . S8.dropWhile (== '\n'))
    takePrefix :: Bool -> ByteString -> ByteString
    takePrefix bracketUsed txt =
      case S8.uncons txt of
        Nothing -> ""
        Just ('>', txt') ->
          if not bracketUsed
            then S8.cons '>' (takePrefix True txt')
            else ""
        Just (c, txt') ->
          if c == ' ' || c == '\t'
            then S8.cons c (takePrefix bracketUsed txt')
            else ""
    findSmallestPrefix :: [ByteString] -> ByteString
    findSmallestPrefix [] = ""
    findSmallestPrefix ("":_) = ""
    findSmallestPrefix (p:ps) =
      let first = S8.head p
          startsWithChar c x = S8.length x > 0 && S8.head x == c
       in if all (startsWithChar first) ps
            then S8.cons first (findSmallestPrefix (S.tail p : map S.tail ps))
            else ""
    preserveTrailingNewline f x
      | S8.null x || S8.all isSpace x = return mempty
      | otherwise =
        if hasTrailingLine x || configTrailingNewline config
          then fmap
                 (\x' ->
                    if hasTrailingLine (L.toStrict (S.toLazyByteString x'))
                      then x'
                      else x' <> "\n")
                 (f x)
          else f x

-- | Generate an AST from the given module for debugging.
testAst :: ByteString -> Either String HsModule
testAst x =
  case parseModule Nothing opts (UTF8.toString x) of
    POk _ m   -> Right $ modifyASTForPrettyPrinting m
    PFailed _ -> Left "Parse failed."
  where
    opts = parserOptsFromExtensions $ CE.uniqueExtensions allExtensions

-- | Does the strict bytestring have a trailing newline?
hasTrailingLine :: ByteString -> Bool
hasTrailingLine xs = not (S8.null xs) && S8.last xs == '\n'

-- | Print the module.
prettyPrint :: Config -> HsModule -> Builder
prettyPrint config m =
  runPrinterStyle config (pretty $ modifyASTForPrettyPrinting m)

-- | Pretty print the given printable thing.
runPrinterStyle :: Config -> Printer () -> Builder
runPrinterStyle config m =
  maybe
    (error "Printer failed with mzero call.")
    psOutput
    (runIdentity
       (runMaybeT
          (execStateT
             (runPrinter m)
             (PrintState
                { psIndentLevel = 0
                , psOutput = mempty
                , psNewline = False
                , psColumn = 0
                , psLine = 1
                , psConfig = config
                , psEolComment = False
                }))))

allExtensions :: [Cabal.Extension]
allExtensions = fmap Cabal.EnableExtension [minBound ..]

-- | Default extensions.
defaultExtensions :: [Cabal.Extension]
defaultExtensions = fmap Cabal.EnableExtension $ [minBound ..] \\ badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [Cabal.KnownExtension]
badExtensions =
  [ Cabal.Arrows -- steals proc
  , Cabal.TransformListComp -- steals the group keyword
  , Cabal.XmlSyntax
  , Cabal.RegularPatterns -- steals a-b
  , Cabal.UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
  , Cabal.PatternSynonyms -- steals the pattern keyword
  , Cabal.RecursiveDo -- steals the rec keyword
  , Cabal.DoRec -- same
  , Cabal.TypeApplications -- since GHC
  ] ++
  badExtensionsSinceGhc941

-- | Additionally disabled extensions since GHC 9.4.1.
--
-- With these extensions enabled, a few tests fail.
badExtensionsSinceGhc941 :: [Cabal.KnownExtension]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
badExtensionsSinceGhc941 =
  [ Cabal.OverloadedRecordUpdate
  , Cabal.AlternativeLayoutRule
  , Cabal.AlternativeLayoutRuleTransitional
  ]
#else
badExtensionsSinceGhc941 = []
#endif
s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
  | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
  | otherwise = Nothing

--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint
-- | Consume an extensions list from arguments.
getExtensions :: [Text] -> [Cabal.Extension]
getExtensions = foldl f defaultExtensions . map T.unpack
  where
    f _ "Haskell98" = []
    f a ('N':'o':x)
      | Just x' <- readExtension x = delete x' a
    f a x
      | Just x' <- readExtension x = x' : delete x' a
    f _ x = error $ "Unknown extension: " ++ x

-- | This function generates a 'ParserOpts' from te given extension. The
-- 'StarIsType' extension is always enabled to compile a code using kinds
-- like '* -> *'.
parserOptsFromExtensions :: [GLP.Extension] -> ParserOpts
#if MIN_VERSION_ghc_lib_parser(9,4,1)
parserOptsFromExtensions opts =
  mkParserOpts
    opts'
    diagOpts
    [] -- There are no supported languages and extensions (this list is used only in error messages)
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
  where
    opts' = ES.fromList $ GLP.StarIsType : opts
    diagOpts =
      DiagOpts
        { diag_warning_flags = ES.empty
        , diag_fatal_warning_flags = ES.empty
        , diag_warn_is_error = False
        , diag_reverse_errors = False
        , diag_max_errors = Nothing
        , diag_ppr_ctx = defaultSDocContext
        }
#else
parserOptsFromExtensions opts =
  mkParserOpts
    ES.empty -- No compiler warnings are enabled.
    opts'
    False -- Safe imports are off.
    False -- Haddock comments are treated as normal comments.
    True -- Comments are kept in an AST.
    False -- Do not update the internal position of a comment.
  where
    opts' = ES.fromList $ GLP.StarIsType : opts
#endif
-- | This function parses the given Haskell source code with the given file
-- path (if any) and parse options.
parseModule :: Maybe FilePath -> ParserOpts -> String -> ParseResult HsModule
parseModule filepath opts src =
  case unP GLP.parseModule initState of
    POk s m   -> POk s $ unLoc m
    PFailed s -> PFailed s
  where
    initState = initParserState opts buffer location
    location =
      mkRealSrcLoc (mkFastString $ fromMaybe "<interactive>" filepath) 1 1
    buffer = stringToStringBuffer src
