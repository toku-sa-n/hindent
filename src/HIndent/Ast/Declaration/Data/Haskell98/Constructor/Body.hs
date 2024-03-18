{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Data.Haskell98.Constructor.Body
  ( Haskell98ConstructorBody
  , mkHaskell98ConstructorBody
  , isRecord
  ) where

import HIndent.Ast.NodeComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import {-# SOURCE #-} HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

data Haskell98ConstructorBody
  = Infix
      { name :: GHC.LIdP GHC.GhcPs
      , left :: GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs)
      , right :: GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs)
      }
  | Prefix
      { name :: GHC.LIdP GHC.GhcPs
      , types :: [GHC.HsScaled GHC.GhcPs (GHC.LBangType GHC.GhcPs)]
      }
  | Record
      { name :: GHC.LIdP GHC.GhcPs
      , record :: GHC.XRec GHC.GhcPs [GHC.LConDeclField GHC.GhcPs]
      }

instance CommentExtraction Haskell98ConstructorBody where
  nodeComments Infix {} = NodeComments [] [] []
  nodeComments Prefix {} = NodeComments [] [] []
  nodeComments Record {} = NodeComments [] [] []

instance Pretty Haskell98ConstructorBody where
  pretty' Infix {..} =
    spaced [pretty left, pretty $ fmap InfixOp name, pretty right]
  pretty' Prefix {..} = pretty name >> hor <-|> ver
    where
      hor = spacePrefixed $ fmap pretty types
      ver = indentedBlock $ newlinePrefixed $ fmap pretty types
  pretty' Record {..} = do
    pretty name
    printCommentsAnd record $ \r ->
      newline >> indentedBlock (vFields $ fmap pretty r)

mkHaskell98ConstructorBody ::
     GHC.ConDecl GHC.GhcPs -> Maybe Haskell98ConstructorBody
mkHaskell98ConstructorBody GHC.ConDeclH98 { con_args = GHC.InfixCon left right
                                          , ..
                                          } = Just Infix {name = con_name, ..}
mkHaskell98ConstructorBody GHC.ConDeclH98 {con_args = GHC.PrefixCon _ types, ..} =
  Just Prefix {name = con_name, ..}
mkHaskell98ConstructorBody GHC.ConDeclH98 {con_args = GHC.RecCon record, ..} =
  Just Record {name = con_name, ..}
mkHaskell98ConstructorBody GHC.ConDeclGADT {} = Nothing

isRecord :: Haskell98ConstructorBody -> Bool
isRecord Record {} = True
isRecord _ = False
