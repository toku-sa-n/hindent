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
import HIndent.Printer

data PatternSynonym
  = Prefix
      { name :: WithComments PrefixName
      , args :: [WithComments PrefixName]
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }
  | Infix
      { leftArg :: WithComments PrefixName
      , operator :: WithComments InfixName
      , rightArg :: WithComments PrefixName
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }
  | Record
      { name :: WithComments PrefixName
      , fields :: [WithComments FieldName]
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }

instance Pretty PatternSynonym where
  pretty Prefix {..} = do
    string "pattern "
    case args of
      [] -> pretty name
      _ -> spaced $ pretty name : fmap pretty args
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty Infix {..} = do
    string "pattern "
    spaced [pretty leftArg, pretty operator, pretty rightArg]
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty Record {..} = do
    string "pattern "
    spaced [pretty name, hFields $ fmap pretty fields]
    prettySuffix isImplicitBidirectional definition explicitMatches

mkPatternSynonym :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> PatternSynonym
#if MIN_VERSION_ghc_lib_parser(9, 14, 0)
mkPatternSynonym psb =
  let (isImplicitBidirectional, explicitMatches) =
        case GHC.psb_dir psb of
          GHC.Unidirectional -> (False, Nothing)
          GHC.ImplicitBidirectional -> (True, Nothing)
          GHC.ExplicitBidirectional matches ->
            (False, Just $ mkExprMatchGroup matches)
      definition = mkPatInsidePatDecl <$> fromGenLocated (GHC.psb_def psb)
   in case GHC.psb_args psb of
        GHC.PrefixCon prefixArgs ->
          Prefix
            { name = fromGenLocated $ mkPrefixName <$> GHC.psb_id psb
            , args = map (fromGenLocated . fmap mkPrefixName) prefixArgs
            , isImplicitBidirectional = isImplicitBidirectional
            , explicitMatches = explicitMatches
            , definition = definition
            }
        GHC.InfixCon leftArg rightArg ->
          Infix
            { leftArg = fromGenLocated $ mkPrefixName <$> leftArg
            , operator = fromGenLocated $ mkInfixName <$> GHC.psb_id psb
            , rightArg = fromGenLocated $ mkPrefixName <$> rightArg
            , isImplicitBidirectional = isImplicitBidirectional
            , explicitMatches = explicitMatches
            , definition = definition
            }
        GHC.RecCon recordFields ->
          Record
            { name = fromGenLocated $ mkPrefixName <$> GHC.psb_id psb
            , fields =
                map
                  (mkWithComments
                     . mkFieldNameFromFieldOcc
                     . GHC.recordPatSynField)
                  recordFields
            , isImplicitBidirectional = isImplicitBidirectional
            , explicitMatches = explicitMatches
            , definition = definition
            }
#else
mkPatternSynonym psb =
  let (isImplicitBidirectional, explicitMatches) =
        case GHC.psb_dir psb of
          GHC.Unidirectional -> (False, Nothing)
          GHC.ImplicitBidirectional -> (True, Nothing)
          GHC.ExplicitBidirectional matches ->
            (False, Just $ mkExprMatchGroup matches)
      definition = mkPatInsidePatDecl <$> fromGenLocated (GHC.psb_def psb)
   in case GHC.psb_args psb of
        GHC.PrefixCon _ prefixArgs ->
          Prefix
            { name = fromGenLocated $ mkPrefixName <$> GHC.psb_id psb
            , args = map (fromGenLocated . fmap mkPrefixName) prefixArgs
            , isImplicitBidirectional = isImplicitBidirectional
            , explicitMatches = explicitMatches
            , definition = definition
            }
        GHC.InfixCon leftArg rightArg ->
          Infix
            { leftArg = fromGenLocated $ mkPrefixName <$> leftArg
            , operator = fromGenLocated $ mkInfixName <$> GHC.psb_id psb
            , rightArg = fromGenLocated $ mkPrefixName <$> rightArg
            , isImplicitBidirectional = isImplicitBidirectional
            , explicitMatches = explicitMatches
            , definition = definition
            }
        GHC.RecCon recordFields ->
          Record
            { name = fromGenLocated $ mkPrefixName <$> GHC.psb_id psb
            , fields =
                map
                  (mkWithComments
                     . mkFieldNameFromFieldOcc
                     . GHC.recordPatSynField)
                  recordFields
            , isImplicitBidirectional = isImplicitBidirectional
            , explicitMatches = explicitMatches
            , definition = definition
            }
#endif
prettySuffix ::
     Bool -> WithComments PatInsidePatDecl -> Maybe MatchGroup -> Printer ()
prettySuffix isImplicitBidirectional definition explicitMatches = do
  let arrow =
        if isImplicitBidirectional
          then "="
          else "<-"
  spacePrefixed [string arrow, pretty definition]
  whenJust explicitMatches $ \matches -> do
    newline
    indentedBlock $ string "where " |=> pretty matches
