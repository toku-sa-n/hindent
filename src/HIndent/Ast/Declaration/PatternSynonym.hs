{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HIndent.Ast.Declaration.PatternSynonym
  ( PatternSynonym
  , mkPatternSynonym
  ) where

import Data.Data
import Data.Maybe
import Data.Monoid
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
  = PrefixPatternSynonym
      { name :: WithComments PrefixName
      , args :: [WithComments PrefixName]
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }
  | InfixPatternSynonym
      { leftArg :: WithComments PrefixName
      , operator :: WithComments InfixName
      , rightArg :: WithComments PrefixName
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }
  | RecordPatternSynonym
      { name :: WithComments PrefixName
      , fields :: [WithComments FieldName]
      , isImplicitBidirectional :: Bool
      , explicitMatches :: Maybe MatchGroup
      , definition :: WithComments PatInsidePatDecl
      }

instance Pretty PatternSynonym where
  pretty PrefixPatternSynonym {..} = do
    string "pattern "
    case args of
      [] -> pretty name
      _ -> spaced $ pretty name : fmap pretty args
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty InfixPatternSynonym {..} = do
    string "pattern "
    spaced [pretty leftArg, pretty operator, pretty rightArg]
    prettySuffix isImplicitBidirectional definition explicitMatches
  pretty RecordPatternSynonym {..} = do
    string "pattern "
    spaced [pretty name, hFields $ fmap pretty fields]
    prettySuffix isImplicitBidirectional definition explicitMatches

mkPatternSynonym :: GHC.PatSynBind GHC.GhcPs GHC.GhcPs -> PatternSynonym
mkPatternSynonym psb =
  case showConstr $ toConstr $ GHC.psb_args psb of
    "PrefixCon" ->
      PrefixPatternSynonym
        { name = fromGenLocated $ mkPrefixName <$> GHC.psb_id psb
        , args = map (fromGenLocated . fmap mkPrefixName) prefixArgs
        , isImplicitBidirectional = isImplicitBidirectional
        , explicitMatches = explicitMatches
        , definition = definition
        }
    "InfixCon" ->
      InfixPatternSynonym
        { leftArg = fromGenLocated $ mkPrefixName <$> left
        , operator = fromGenLocated $ mkInfixName <$> GHC.psb_id psb
        , rightArg = fromGenLocated $ mkPrefixName <$> right
        , isImplicitBidirectional = isImplicitBidirectional
        , explicitMatches = explicitMatches
        , definition = definition
        }
    "RecCon" ->
      RecordPatternSynonym
        { name = fromGenLocated $ mkPrefixName <$> GHC.psb_id psb
        , fields =
            map
              (mkWithComments . mkFieldNameFromFieldOcc . GHC.recordPatSynField)
              recordFields
        , isImplicitBidirectional = isImplicitBidirectional
        , explicitMatches = explicitMatches
        , definition = definition
        }
    _ -> error "Unsupported pattern synonym details."
  where
    (isImplicitBidirectional, explicitMatches) =
      case GHC.psb_dir psb of
        GHC.Unidirectional -> (False, Nothing)
        GHC.ImplicitBidirectional -> (True, Nothing)
        GHC.ExplicitBidirectional matches ->
          (False, Just $ mkExprMatchGroup matches)
    definition = mkPatInsidePatDecl <$> fromGenLocated (GHC.psb_def psb)
    prefixArgs =
      fromMaybe []
        $ getFirst
        $ gmapQl
            (<>)
            mempty
            (First
               . (cast :: forall d. Data d => d -> Maybe [GHC.LIdP GHC.GhcPs]))
            (GHC.psb_args psb)
    (left, right) =
      case infixArgs of
        leftArg:rightArg:_ -> (leftArg, rightArg)
        _ -> error "Unsupported infix pattern synonym details."
    infixArgs =
      gmapQl
        (++)
        []
        (maybeToList
           . (cast :: forall d. Data d => d -> Maybe (GHC.LIdP GHC.GhcPs)))
        (GHC.psb_args psb)
    recordFields =
      fromMaybe []
        $ getFirst
        $ gmapQl
            (<>)
            mempty
            (First
               . (cast :: forall d. Data d =>
                                      d -> Maybe
                                             [GHC.RecordPatSynField GHC.GhcPs]))
            (GHC.psb_args psb)

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
