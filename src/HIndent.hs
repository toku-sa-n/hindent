{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, PatternGuards #-}

-- | Haskell indenter.

module HIndent
  (-- * Formatting functions.
   reformat
  ,prettyPrint
  ,parseMode
  -- * Testing
  ,test
  ,testFile
  ,testAst
  ,testFileAst
  ,defaultExtensions
  ,getExtensions
  )
  where

import           Control.Monad.State.Strict
import           Control.Monad.Trans.Maybe
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Unsafe as S
import           Data.Char
import           Data.Foldable (foldr')
import           Data.Either
import           Data.Function
import           Data.Functor.Identity
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable hiding (mapM)
import           HIndent.CodeBlock
import           HIndent.Pretty
import           HIndent.Types
import qualified Language.Haskell.Exts as Exts hiding (unLoc)
import           Language.Haskell.Extension (Extension, Extension(..), KnownExtension(..))
import           Language.Haskell.Exts hiding (unLoc, Style, prettyPrint, Pretty, style, parse, Extension(..), EnableExtension, KnownExtension(..), ParseResult, parseModule, parseModuleWithComments)
import           Prelude
import qualified SwitchToGhcLibParserHelper as Helper
import GHC.Hs
import GHC.Parser.Lexer
import GHC.Parser
import Generics.SYB.Schemes
import GHC.Types.SrcLoc
import GHC.Data.FastString
import GHC.Data.StringBuffer
import qualified GHC.Data.EnumSet as ES
import qualified ConvertModule as Helper

-- | Format the given source.
reformat :: Config -> Maybe [Extension] -> Maybe FilePath -> ByteString -> Either String Builder
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
            exts = readExtensions (UTF8.toString code)
            allExts = maybe allExtensions (fmap Helper.cabalExtensionToHSEExtension) mexts ++ case exts of
                                                                                                  Just (_, exts') -> fmap Helper.cabalExtensionToHSEExtension (configExtensions config) ++ exts'
                                                                                                  _ -> []
            opts = mkParserOpts ES.empty (ES.fromList $ Helper.uniqueExtensions $ fmap Helper.hseExtensionToCabalExtension allExts) False True True True
        in case parseModuleWithComments mfilepath opts (UTF8.toString code) of
               POk _ (m, comments) ->
                   fmap
                       (S.lazyByteString . addPrefix prefix . S.toLazyByteString)
                       (prettyPrint config (Helper.convertModule m) (mapMaybe Helper.convertComment comments))
               PFailed _ -> Left "Parse failed." -- TODO: Improve this error message.
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
               then S8.cons
                        first
                        (findSmallestPrefix (S.tail p : map S.tail ps))
               else ""
    preserveTrailingNewline f x =
        if S8.null x || S8.all isSpace x
            then return mempty
            else if hasTrailingLine x || configTrailingNewline config
                     then fmap
                              (\x' ->
                                    if hasTrailingLine
                                           (L.toStrict (S.toLazyByteString x'))
                                        then x'
                                        else x' <> "\n")
                              (f x)
                     else f x

-- | Does the strict bytestring have a trailing newline?
hasTrailingLine :: ByteString -> Bool
hasTrailingLine xs =
    if S8.null xs
        then False
        else S8.last xs == '\n'

-- | Print the module.
prettyPrint :: Config
            -> Module SrcSpanInfo
            -> [Comment]
            -> Either a Builder
prettyPrint config m comments =
  let ast =
        evalState
          (collectAllComments
             (fromMaybe m (applyFixities baseFixities m)))
          comments
  in Right (runPrinterStyle config (pretty ast))

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
              , psInsideCase = False
              , psFitOnOneLine = False
              , psEolComment = False
              }))))

-- | Parse mode, includes all extensions, doesn't assume any fixities.
parseMode :: ParseMode
parseMode =
  defaultParseMode {extensions = allExtensions
                   ,fixities = Nothing}

allExtensions :: [Exts.Extension]
allExtensions = fmap (Helper.cabalExtensionToHSEExtension . EnableExtension) [minBound ..]

-- | Test the given file.
testFile :: FilePath -> IO ()
testFile fp  = S.readFile fp >>= test

-- | Test the given file.
testFileAst :: FilePath -> IO ()
testFileAst fp  = S.readFile fp >>= print . testAst

-- | Test with the given style, prints to stdout.
test :: ByteString -> IO ()
test =
  either error (L8.putStrLn . S.toLazyByteString) .
  reformat defaultConfig Nothing Nothing

-- | Parse the source and annotate it with comments, yielding the resulting AST.
testAst :: ByteString -> Either String (Module NodeInfo)
testAst x =
  case Exts.parseModuleWithComments parseMode (UTF8.toString x) of
    ParseOk (m,comments) ->
      Right
        (let ast =
               evalState
                 (collectAllComments
                    (fromMaybe m (applyFixities baseFixities m)))
                 comments
         in ast)
    ParseFailed _ e -> Left e

-- | Default extensions.
defaultExtensions :: [Extension]
defaultExtensions = fmap EnableExtension $ [minBound .. ] \\ badExtensions

-- | Extensions which steal too much syntax.
badExtensions :: [KnownExtension]
badExtensions =
    [Arrows -- steals proc
    ,TransformListComp -- steals the group keyword
    ,XmlSyntax, RegularPatterns -- steals a-b
    ,UnboxedTuples -- breaks (#) lens operator
    -- ,QuasiQuotes -- breaks [x| ...], making whitespace free list comps break
    ,PatternSynonyms -- steals the pattern keyword
    ,RecursiveDo -- steals the rec keyword
    ,DoRec -- same
    ,TypeApplications -- since GHC 8 and haskell-src-exts-1.19
    ]


s8_stripPrefix :: ByteString -> ByteString -> Maybe ByteString
s8_stripPrefix bs1@(S.PS _ _ l1) bs2
   | bs1 `S.isPrefixOf` bs2 = Just (S.unsafeDrop l1 bs2)
   | otherwise = Nothing

--------------------------------------------------------------------------------
-- Extensions stuff stolen from hlint

-- | Consume an extensions list from arguments.
getExtensions :: [Text] -> [Extension]
getExtensions = foldl f defaultExtensions . map T.unpack
  where f _ "Haskell98" = []
        f a ('N':'o':x)
          | Just x' <- readExtension x =
            delete x' a
        f a x
          | Just x' <- readExtension x =
            x' :
            delete x' a
        f _ x = error $ "Unknown extension: " ++ x

--------------------------------------------------------------------------------
-- Comments

-- | Traverse the structure backwards.
traverseInOrder
  :: (Monad m, Traversable t, Functor m)
  => (b -> b -> Ordering) -> (b -> m b) -> t b -> m (t b)
traverseInOrder cmp f ast = do
  indexed <-
    fmap (zip [0 :: Integer ..] . reverse) (execStateT (traverse (modify . (:)) ast) [])
  let sorted = sortBy (\(_,x) (_,y) -> cmp x y) indexed
  results <-
    mapM
      (\(i,m) -> do
         v <- f m
         return (i, v))
      sorted
  evalStateT
    (traverse
       (const
          (do i <- gets head
              modify tail
              case lookup i results of
                Nothing -> error "traverseInOrder"
                Just x -> return x))
       ast)
    [0 ..]

-- | Collect all comments in the module by traversing the tree. Read
-- this from bottom to top.
collectAllComments :: Module SrcSpanInfo -> State [Comment] (Module NodeInfo)
collectAllComments =
  shortCircuit
    (traverseBackwards
     -- Finally, collect backwards comments which come after each node.
       (collectCommentsBy
          CommentAfterLine
          (\nodeSpan commentSpan ->
              Exts.srcSpanStartLine commentSpan >= Exts.srcSpanEndLine nodeSpan))) <=<
  shortCircuit addCommentsToTopLevelWhereClauses <=<
  shortCircuit
    (traverse
     -- Collect forwards comments which start at the end line of a
     -- node: Does the start line of the comment match the end-line
     -- of the node?
       (collectCommentsBy
          CommentSameLine
          (\nodeSpan commentSpan ->
              Exts.srcSpanStartLine commentSpan == Exts.srcSpanEndLine nodeSpan))) <=<
  shortCircuit
    (traverseBackwards
     -- Collect backwards comments which are on the same line as a
     -- node: Does the start line & end line of the comment match
     -- that of the node?
       (collectCommentsBy
          CommentSameLine
          (\nodeSpan commentSpan ->
              Exts.srcSpanStartLine commentSpan == Exts.srcSpanStartLine nodeSpan &&
              Exts.srcSpanStartLine commentSpan == Exts.srcSpanEndLine nodeSpan))) <=<
  shortCircuit
    (traverse
     -- First, collect forwards comments for declarations which both
     -- start on column 1 and occur before the declaration.
       (collectCommentsBy
          CommentBeforeLine
          (\nodeSpan commentSpan ->
              (Exts.srcSpanStartColumn nodeSpan == 1 &&
               Exts.srcSpanStartColumn commentSpan == 1) &&
              Exts.srcSpanStartLine commentSpan < Exts.srcSpanStartLine nodeSpan))) .
  fmap (nodify . Helper.fromHSESrcSpanInfo)
  where
    nodify s = NodeInfo s mempty
    -- Sort the comments by their end position.
    traverseBackwards =
      traverseInOrder
        (\x y -> on (flip compare) (Exts.srcSpanEnd . Helper.srcInfoSpan . nodeInfoSpan) x y)
    -- Stop traversing if all comments have been consumed.
    shortCircuit m v = do
      comments <- get
      if null comments
        then return v
        else m v

-- | Collect comments by satisfying the given predicate, to collect a
-- comment means to remove it from the pool of available comments in
-- the State. This allows for a multiple pass approach.
collectCommentsBy
  :: (Helper.SrcSpan -> SomeComment -> NodeComment)
  -> (Exts.SrcSpan -> Exts.SrcSpan -> Bool)
  -> NodeInfo
  -> State [Comment] NodeInfo
collectCommentsBy cons predicate nodeInfo@(NodeInfo (Helper.SrcSpanInfo nodeSpan) _) = do
  comments <- get
  let (others, mine) =
        partitionEithers
          (map
             (\comment@(Comment _ commentSpan _) ->
                 if predicate nodeSpan commentSpan
                   then Right comment
                   else Left comment)
             comments)
  put others
  return $ addCommentsToNode cons mine nodeInfo

-- | Reintroduce comments which were immediately above declarations in where clauses.
-- Affects where clauses of top level declarations only.
addCommentsToTopLevelWhereClauses ::
     Module NodeInfo -> State [Comment] (Module NodeInfo)
addCommentsToTopLevelWhereClauses (Module x x' x'' x''' topLevelDecls) =
  Module x x' x'' x''' <$>
  traverse addCommentsToWhereClauses topLevelDecls
  where
    addCommentsToWhereClauses ::
         Decl NodeInfo -> State [Comment] (Decl NodeInfo)
    addCommentsToWhereClauses (Exts.PatBind x x' x'' (Just (BDecls x''' whereDecls))) = do
      newWhereDecls <- traverse addCommentsToPatBind whereDecls
      return $ Exts.PatBind x x' x'' (Just (BDecls x''' newWhereDecls))
    addCommentsToWhereClauses other = return other
    addCommentsToPatBind :: Decl NodeInfo -> State [Comment] (Decl NodeInfo)
    addCommentsToPatBind (Exts.PatBind bindInfo (PVar x (Ident declNodeInfo declString)) x' x'') = do
      bindInfoWithComments <- addCommentsBeforeNode bindInfo
      return $
        Exts.PatBind
          bindInfoWithComments
          (PVar x (Ident declNodeInfo declString))
          x'
          x''
    addCommentsToPatBind other = return other
    addCommentsBeforeNode :: NodeInfo -> State [Comment] NodeInfo
    addCommentsBeforeNode nodeInfo = do
      comments <- get
      let (notAbove, above) = partitionAboveNotAbove comments nodeInfo
      put notAbove
      return $ addCommentsToNode CommentBeforeLine above nodeInfo
    partitionAboveNotAbove :: [Comment] -> NodeInfo -> ([Comment], [Comment])
    partitionAboveNotAbove cs (NodeInfo (Helper.SrcSpanInfo nodeSpan) _) =
      fst $
      foldr'
        (\comment@(Comment _ commentSpan _) ((ls, rs), lastSpan) ->
           if comment `isAbove` lastSpan
             then ((ls, comment : rs), commentSpan)
             else ((comment : ls, rs), lastSpan))
        (([], []), nodeSpan)
        cs
    isAbove :: Comment -> Exts.SrcSpan -> Bool
    isAbove (Comment _ commentSpan _) span =
      let (_, commentColStart) = Exts.srcSpanStart commentSpan
          (commentLnEnd, _) = Exts.srcSpanEnd commentSpan
          (lnStart, colStart) = Exts.srcSpanStart span
       in commentColStart == colStart && commentLnEnd + 1 == lnStart
addCommentsToTopLevelWhereClauses other = return other

addCommentsToNode :: (Helper.SrcSpan -> SomeComment -> NodeComment)
                  -> [Comment]
                  -> NodeInfo
                  -> NodeInfo
addCommentsToNode mkNodeComment newComments nodeInfo@(NodeInfo _ existingComments) =
  nodeInfo
    {nodeInfoComments = existingComments <> map mkBeforeNodeComment newComments}
  where
    mkBeforeNodeComment :: Comment -> NodeComment
    mkBeforeNodeComment (Comment multiLine commentSpan commentString) =
      mkNodeComment
        (Helper.fromHSESrcSpan commentSpan)
        ((if multiLine
            then MultiLine
            else EndOfLine)
           commentString)

parseModuleWithComments :: Maybe FilePath -> ParserOpts -> String -> ParseResult (HsModule, [LEpaComment])
parseModuleWithComments filepath opts src =
    case unP parseModule initState of
        POk s m -> POk s (unLoc m, listify onlyComments $ unLoc m)
        PFailed s -> PFailed s
    where
      onlyComments :: LEpaComment -> Bool
      onlyComments _ = True
      initState = initParserState opts buffer location
      location = mkRealSrcLoc (mkFastString (fromMaybe "<interactive>" filepath)) 1 1
      buffer = stringToStringBuffer src
