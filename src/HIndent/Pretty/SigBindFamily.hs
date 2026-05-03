{-# LANGUAGE LambdaCase #-}

-- | A module defining 'SigBindFamily' and other related types and
-- functions.
module HIndent.Pretty.SigBindFamily
  ( SigBindFamily(..)
  , LSigBindFamily
  , mkSortedLSigBindFamilyList
  , mkLSigBindFamilyList
  , destructLSigBindFamilyList
  , filterLSig
  , filterLBind
  ) where

import Data.Function
import Data.List (sortBy)
import Data.Maybe
import GHC.Hs
import GHC.Types.SrcLoc
import {-# SOURCE #-} HIndent.Ast.Declaration.Bind (prettyBind)
import HIndent.Ast.Declaration.Family.Data
import HIndent.Ast.Declaration.Family.Type
import HIndent.Ast.Declaration.Instance.Family.Data.Associated
  ( mkAssociatedDataFamilyInstance
  )
import HIndent.Ast.Declaration.Instance.Family.Type.Associated
import HIndent.Ast.Declaration.Instance.Family.Type.Associated.Default
import HIndent.Ast.Declaration.Signature
import HIndent.Pretty

-- | A sum type containing one of those: function signature, function
-- binding, family declaration (type or data), type family instance, and data family instance.
data SigBindFamily
  = Sig (Sig GhcPs)
  | Bind (HsBindLR GhcPs GhcPs)
  | Family (FamilyDecl GhcPs)
  | TyFamInst (TyFamInstDecl GhcPs)
  | TyFamDeflt (TyFamDefltDecl GhcPs)
  | DataFamInst (DataFamInstDecl GhcPs)

instance Pretty SigBindFamily where
  pretty (Sig signature) = pretty $ mkSignature signature
  pretty (Bind bind) = prettyBind bind
  pretty (Family familyDecl)
    | Just typeFamily <- mkTypeFamily familyDecl = pretty typeFamily
    | Just dataFamily <- mkDataFamily familyDecl = pretty dataFamily
    | otherwise = error "Unreachable"
  pretty (TyFamInst inst) = pretty $ mkAssociatedType inst
  pretty (TyFamDeflt deflt) = pretty $ mkAssociatedTypeDefault deflt
  pretty (DataFamInst inst) = pretty $ mkAssociatedDataFamilyInstance inst

-- | 'SigBindFamily' with the location information.
type LSigBindFamily = GenLocated SrcSpanAnnA SigBindFamily

-- | Creates a list of 'LSigBindFamily' from arguments. The list is sorted
-- by its elements' locations.
mkSortedLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LTyFamDefltDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkSortedLSigBindFamilyList sigs binds fams insts deflts datafams =
  sortBy (compare `on` realSrcSpan . locA . getLoc)
    $ mkLSigBindFamilyList sigs binds fams insts deflts datafams

-- | Creates a list of 'LSigBindFamily' from arguments.
mkLSigBindFamilyList ::
     [LSig GhcPs]
  -> [LHsBindLR GhcPs GhcPs]
  -> [LFamilyDecl GhcPs]
  -> [LTyFamInstDecl GhcPs]
  -> [LTyFamDefltDecl GhcPs]
  -> [LDataFamInstDecl GhcPs]
  -> [LSigBindFamily]
mkLSigBindFamilyList sigs binds fams insts deflts datafams =
  fmap (fmap Sig) sigs
    ++ fmap (fmap Bind) binds
    ++ fmap (fmap Family) fams
    ++ fmap (fmap TyFamInst) insts
    ++ fmap (fmap TyFamDeflt) deflts
    ++ fmap (fmap DataFamInst) datafams

-- | Destructs a list of 'LSigBindFamily'
destructLSigBindFamilyList ::
     [LSigBindFamily]
  -> ( [LSig GhcPs]
     , [LHsBindLR GhcPs GhcPs]
     , [LFamilyDecl GhcPs]
     , [LTyFamInstDecl GhcPs]
     , [LTyFamDefltDecl GhcPs]
     , [LDataFamInstDecl GhcPs])
destructLSigBindFamilyList =
  (,,,,,)
    <$> filterLSig
    <*> filterLBind
    <*> filterLFamily
    <*> filterLTyFamInst
    <*> filterLTyFamDeflt
    <*> filterLDataFamInst

-- | Filters out @Sig@s and extract the wrapped values.
filterLSig :: [LSigBindFamily] -> [LSig GhcPs]
filterLSig =
  mapMaybe
    (\case
       (L l (Sig x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'Bind's and extract the wrapped values.
filterLBind :: [LSigBindFamily] -> [LHsBindLR GhcPs GhcPs]
filterLBind =
  mapMaybe
    (\case
       (L l (Bind x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'Family's and extract the wrapped values.
filterLFamily :: [LSigBindFamily] -> [LFamilyDecl GhcPs]
filterLFamily =
  mapMaybe
    (\case
       (L l (Family x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'TyFamInst's and extract the wrapped values.
filterLTyFamInst :: [LSigBindFamily] -> [LTyFamInstDecl GhcPs]
filterLTyFamInst =
  mapMaybe
    (\case
       (L l (TyFamInst x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'TyFamDeflt's and extract the wrapped values.
filterLTyFamDeflt :: [LSigBindFamily] -> [LTyFamDefltDecl GhcPs]
filterLTyFamDeflt =
  mapMaybe
    (\case
       (L l (TyFamDeflt x)) -> Just $ L l x
       _ -> Nothing)

-- | Filters out 'DataFamInst's and extract the wrapped values.
filterLDataFamInst :: [LSigBindFamily] -> [LDataFamInstDecl GhcPs]
filterLDataFamInst =
  mapMaybe
    (\case
       (L l (DataFamInst x)) -> Just $ L l x
       _ -> Nothing)
