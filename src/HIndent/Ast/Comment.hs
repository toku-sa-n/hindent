{-# LANGUAGE OverloadedStrings #-}

module HIndent.Ast.Comment
  ( Comment
  , mkComment
  ) where

import qualified Data.Text as Text
import qualified GHC.Hs as GHC
import HIndent.Ast.TextValue
import HIndent.Pretty
import HIndent.Pretty.Combinators

data Comment
  = Line TextValue
  | Block TextValue
  deriving (Eq, Show)

instance Pretty Comment where
  pretty (Line c) = pretty c
  pretty (Block c) =
    case Text.lines $ toText c of
      [] -> pure ()
      [x] -> string x
      (x:xs) -> do
        string x
        newline
        -- 'indentedWithFixedLevel 0' is used because a 'BlockComment'
        -- contains indent spaces for all lines except the first one.
        indentedWithFixedLevel 0 $ lined $ fmap string xs

mkComment :: GHC.EpaCommentTok -> Comment
mkComment (GHC.EpaLineComment c) = Line $ mkTextValueFromString c
mkComment (GHC.EpaBlockComment c) = Block $ mkTextValueFromString c
mkComment _ = Line $ mkTextValueFromString ""
