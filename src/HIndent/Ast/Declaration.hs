{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration
  ( DeclarationCollection
  , mkDeclarationCollection
  ) where

import Data.Maybe
import GHC.Hs
import GHC.Types.SrcLoc
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types

newtype DeclarationCollection =
  DeclarationCollection [LHsDecl GhcPs]

instance CommentExtraction DeclarationCollection where
  nodeComments (DeclarationCollection _) = NodeComments [] [] []

instance Pretty DeclarationCollection where
  pretty' (DeclarationCollection decls) = prettyDecls
    where
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp)
          $ addDeclSeparator decls
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {}) = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator (SigD _ PatSynSig {}) = newline
      declSeparator _ = blankline
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
mkDeclarationCollection :: HsModule GhcPs -> DeclarationCollection
#else
mkDeclarationCollection :: HsModule -> DeclarationCollection
#endif
mkDeclarationCollection HsModule {..} = DeclarationCollection hsmodDecls
