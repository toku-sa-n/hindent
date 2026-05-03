{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.Instance.Class
  ( ClassInstance
  , mkClassInstance
  ) where

import Control.Monad
import HIndent.Applicative
import HIndent.Ast.Declaration.Instance.Class.OverlapMode
import HIndent.Ast.Type (InstDeclType, mkInstDeclType)
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import HIndent.Pretty.SigBindFamily
#if !MIN_VERSION_ghc_lib_parser(9, 12, 1)
import qualified GHC.Data.Bag as GHC
#endif
data ClassInstance = ClassInstance
  { overlapMode :: Maybe (WithComments OverlapMode)
  , body :: [WithComments SigBindFamily]
  , cid_poly_ty :: WithComments InstDeclType
  }

instance Pretty ClassInstance where
  pretty (ClassInstance {..}) = do
    string "instance " |=> do
      whenJust overlapMode $ \x -> do
        pretty x
        space
      pretty cid_poly_ty |=> unless (null sigsAndMethods) (string " where")
    unless (null sigsAndMethods) $ do
      newline
      indentedBlock $ lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods = body

mkClassInstance :: GHC.InstDecl GHC.GhcPs -> Maybe ClassInstance
#if MIN_VERSION_ghc_lib_parser(9, 12, 1)
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { cid_poly_ty =
            flattenComments $ mkInstDeclType <$> fromGenLocated cid_poly_ty
        , body =
            fmap fromGenLocated
              $ mkSortedLSigBindFamilyList
                  cid_sigs
                  cid_binds
                  []
                  cid_tyfam_insts
                  []
                  cid_datafam_insts
        , overlapMode = fmap mkOverlapMode . fromGenLocated <$> cid_overlap_mode
        }
#else
mkClassInstance GHC.ClsInstD {cid_inst = GHC.ClsInstDecl {..}} =
  Just
    $ ClassInstance
        { cid_poly_ty =
            flattenComments $ mkInstDeclType <$> fromGenLocated cid_poly_ty
        , body =
            fromGenLocated
              <$> mkSortedLSigBindFamilyList
                    cid_sigs
                    (GHC.bagToList cid_binds)
                    []
                    cid_tyfam_insts
                    []
                    cid_datafam_insts
        , overlapMode = fmap mkOverlapMode . fromGenLocated <$> cid_overlap_mode
        }
#endif
mkClassInstance _ = Nothing
