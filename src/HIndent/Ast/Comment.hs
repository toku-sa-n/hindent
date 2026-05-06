{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Comment
  ( Comment
  , getColumn
  , mkComment
  ) where

import qualified Data.Text as Text
import qualified GHC.Hs as GHC
import qualified GHC.Types.SrcLoc as GHC
import HIndent.Ast.TextValue
import qualified HIndent.GhcLibParserWrapper.GHC.Parser.Annotation as Annotation
import HIndent.Pretty
import HIndent.Pretty.Combinators

data Comment
  = Line
      { text :: TextValue
      , column :: Maybe Int
      }
  | Block
      { text :: TextValue
      , column :: Maybe Int
      }
  deriving (Eq, Show)

instance Pretty Comment where
  pretty Line {..} = pretty text
  pretty Block {..} =
    case Text.lines $ toText text of
      [] -> pure ()
      [x] -> string x
      (x:xs) -> do
        string x
        newline
        -- 'indentedWithFixedLevel 0' is used because a 'BlockComment'
        -- contains indent spaces for all lines except the first one.
        indentedWithFixedLevel 0 $ lined $ fmap string xs

mkComment :: GHC.LEpaComment -> Comment
mkComment comment@(GHC.L _ (GHC.EpaComment (GHC.EpaLineComment text) _)) =
  Line {text = mkTextValueFromString text, column = Just $ getColumn' comment}
mkComment comment@(GHC.L _ (GHC.EpaComment (GHC.EpaBlockComment text) _)) =
  Block {text = mkTextValueFromString text, column = Just $ getColumn' comment}
mkComment comment@(GHC.L _ _) =
  Line {text = mkTextValueFromString "", column = Just $ getColumn' comment}

getColumn :: Comment -> Maybe Int
getColumn = column

getColumn' :: GHC.LEpaComment -> Int
getColumn' =
  fromIntegral
    . subtract 1
    . GHC.srcSpanStartCol
    . Annotation.epaLocationToRealSrcSpan
    . GHC.getLoc
