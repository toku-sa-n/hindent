{-# LANGUAGE CPP #-}

module HIndent.Ast.Context
  ( Context
  , mkContext
  ) where

import                          HIndent.Ast.NodeComments
import                          HIndent.Ast.Type
import                          HIndent.Ast.WithComments
import                qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-}           HIndent.Pretty
import                          HIndent.Pretty.Combinators
import                          HIndent.Pretty.NodeComments

newtype Context =
  Context (Maybe [WithComments Type])

instance CommentExtraction Context where
  nodeComments (Context _) = NodeComments [] [] []

instance Pretty Context where
  pretty' (Context xs) = hor <-|> ver
    where
      hor = parensConditional $ mapM_ (hCommaSep . fmap pretty) xs
        where
          parensConditional =
            case xs of
              Nothing  -> id
              Just [_] -> id
              Just _   -> parens
      ver =
        case xs of
          Nothing  -> pure ()
          Just []  -> string "()"
          Just [x] -> pretty x
          Just xs' -> vTuple $ fmap pretty xs'

mkContext :: GHC.HsContext GHC.GhcPs -> Context
mkContext = Context . Just . fmap (fmap mkType . fromGenLocated)
