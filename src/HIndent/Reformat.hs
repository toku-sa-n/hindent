{-# LANGUAGE OverloadedStrings #-}

module HIndent.Reformat where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.UTF8 as UTF8
import Data.Char
import Data.Maybe
import GHC.Parser.Lexer hiding (buffer, options)
import GHC.Types.SrcLoc
import HIndent.ByteString
import HIndent.CodeBlock
import HIndent.Config
import HIndent.Error
import HIndent.GhcLibParserWrapper.GHC.Hs
import HIndent.LanguageExtension
import qualified HIndent.LanguageExtension.Conversion as CE
import HIndent.LanguageExtension.Types
import HIndent.ModulePreprocessing
import HIndent.Parse
import HIndent.Pretty
import HIndent.Printer

-- | Format the given source.
reformat ::
     Config
  -> [Extension]
  -> Maybe FilePath
  -> ByteString
  -> Either ParseError ByteString
reformat config mexts mfilepath rawCode =
  preserveTrailingNewline
    (fmap unlines' . mapM processBlock . cppSplitBlocks)
    rawCode
  where
    processBlock :: CodeBlock -> Either ParseError ByteString
    processBlock (Shebang text) = Right text
    processBlock (CPPDirectives text) = Right text
    processBlock (HaskellSource yPos text) =
      let ls = S8.lines text
          prefix = findPrefix ls
          code = unlines' (map (stripPrefix prefix) ls)
       in case parseModule mfilepath allExts (UTF8.toString code) of
            POk _ m ->
              Right
                $ addPrefix prefix
                $ L.toStrict
                $ S.toLazyByteString
                $ prettyPrint config m
            PFailed st ->
              let rawErrLoc = psRealLoc $ loc st
               in Left
                    $ ParseError
                        { errorLine = srcLocLine rawErrLoc + yPos
                        , errorCol = srcLocCol rawErrLoc
                        , errorFile = fromMaybe "<interactive>" mfilepath
                        }
    preserveTrailingNewline f x
      | S8.null x || S8.all isSpace x = return mempty
      | hasTrailingLine x || configTrailingNewline config =
        fmap
          (\x' ->
             if hasTrailingLine x'
               then x'
               else x' <> "\n")
          (f x)
      | otherwise = f x
    allExts =
      CE.uniqueExtensions
        $ concatMap (\x -> x : extensionImplies x)
        $ mexts ++ configExtensions config ++ allExtsFromCode
    allExtsFromCode = concatMap f codeBlocks
      where
        f (HaskellSource _ text) =
          collectLanguageExtensionsFromSource $ UTF8.toString text
        f _ = []
    codeBlocks = cppSplitBlocks rawCode

-- | Print the module.
prettyPrint :: Config -> HsModule' -> Builder
prettyPrint config m =
  runPrinterStyle config (pretty $ modifyASTForPrettyPrinting m)
