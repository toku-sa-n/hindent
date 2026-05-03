{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Declaration.PatternSynonym
  ( PatternSynonym
  , mkPatternSynonym
  ) where

import HIndent.Applicative
import HIndent.Ast.MatchGroup (MatchGroup, mkExprMatchGroup)
import HIndent.Ast.Name.Infix
import HIndent.Ast.Name.Prefix
import HIndent.Ast.Name.RecordField (FieldName, mkFieldNameFromFieldOcc)
import HIndent.Ast.Pattern
import HIndent.Ast.WithComments
import qualified HIndent.GhcLibParserWrapper.GHC.Hs as GHC
import HIndent.Pretty
import HIndent.Pretty.Combinators
import qualified HIndent.Printer as Printer

data PatternSynonym
  = Prefix
      (WithComments PrefixName)
      [WithComments PrefixName]
      Bool
      (Maybe MatchGroup)
      (WithComments PatInsidePatDecl)
  | Infix
      (WithComments PrefixName)
      (WithComments InfixName)
      (WithComments PrefixName)
      Bool
      (Maybe MatchGroup)
      (WithComments PatInsidePatDecl)
  | Record
      (WithComments PrefixName)
      [WithComments FieldName]
      Bool
      (Maybe MatchGroup)
      (WithComments PatInsidePatDecl)

instance Pretty PatternSynonym where
  pretty (Prefix name args isImplicitBidirectional explicitMatches definition) = do
    string "pattern "
    case args of
      [] -> pretty name
      _ -> spaced $ pretty name : fmap pretty args
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty (Infix leftArg operator rightArg isImplicitBidirectional explicitMatches definition) = do
    string "pattern "
    spaced [pretty leftArg, pretty operator, pretty rightArg]
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty (Record name fields isImplicitBidirectional explicitMatches definition) = do
    string "pattern "
    spaced [pretty name, hFields $ fmap pretty fields]
    prettySuffix isImplicitBidirectional definition explicitMatches

mkPatternSynonym :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> PatternSynonym
mkPatternSynonym GHC.PSB {..} =
  mkPatternSynonymBody
    psb_id
    psb_args
    isImplicitBidirectional
    explicitMatches
    definition
  where
    (isImplicitBidirectional, explicitMatches) =
      case psb_dir of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
    definition = mkPatInsidePatDecl <$> fromGenLocated psb_def

mkPatternSynonymBody ::
     GHC.LIdP GHC.GhcPs
  -> GHC.HsPatSynDetails GHC.GhcPs
  -> Bool
  -> Maybe MatchGroup
  -> WithComments PatInsidePatDecl
  -> PatternSynonym
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkPatternSynonymBody name (GHC.PrefixCon args) isImplicitBidirectional explicitMatches definition =
  Prefix
    (fromGenLocated $ fmap mkPrefixName name)
    (map (fromGenLocated . fmap mkPrefixName) args)
    isImplicitBidirectional
    explicitMatches
    definition
#else
mkPatternSynonymBody name (GHC.PrefixCon _ args) isImplicitBidirectional explicitMatches definition =
  Prefix
    (fromGenLocated $ fmap mkPrefixName name)
    (map (fromGenLocated . fmap mkPrefixName) args)
    isImplicitBidirectional
    explicitMatches
    definition
#endif
mkPatternSynonymBody name (GHC.InfixCon l r) isImplicitBidirectional explicitMatches definition =
  Infix
    (fromGenLocated $ fmap mkPrefixName l)
    (fromGenLocated $ fmap mkInfixName name)
    (fromGenLocated $ fmap mkPrefixName r)
    isImplicitBidirectional
    explicitMatches
    definition
mkPatternSynonymBody name (GHC.RecCon fields) isImplicitBidirectional explicitMatches definition =
  Record
    (fromGenLocated $ fmap mkPrefixName name)
    (map
       (mkWithComments . mkFieldNameFromFieldOcc . GHC.recordPatSynField)
       fields)
    isImplicitBidirectional
    explicitMatches
    definition

prettySuffix ::
     Bool
  -> WithComments PatInsidePatDecl
  -> Maybe MatchGroup
  -> Printer.Printer ()
prettySuffix isImplicitBidirectional definition explicitMatches = do
  let arrow =
        if isImplicitBidirectional
          then "="
          else "<-"
  spacePrefixed [string arrow, pretty definition]
  whenJust explicitMatches $ \matches -> do
    newline
    indentedBlock $ string "where " |=> pretty matches
