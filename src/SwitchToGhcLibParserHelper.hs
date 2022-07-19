module SwitchToGhcLibParserHelper
  ( cabalExtensionToHSEExtension
  , hseExtensionToCabalExtension
  , SrcSpanInfo(..)
  , fromHSESrcSpanInfo
  , SrcSpan(..)
  , fromHSESrcSpan
  , toHSESrcSpan
  , toExtension
  , gleExtensionToCabalExtension
  , uniqueExtensions
  , convertExtension
  , convertComment
  ) where

import           Data.Maybe
import qualified GHC.Data.FastString             as GLP
import qualified GHC.Hs                          as GLP hiding (UnicodeSyntax)
import qualified GHC.LanguageExtensions          as GLP
import qualified GHC.Types.SrcLoc                as GLP
import qualified Language.Haskell.Extension      as Cabal
import qualified Language.Haskell.Exts           as HSE
import qualified Language.Haskell.Exts.Extension as HSE
import           Text.Read

cabalExtensionToHSEExtension :: Cabal.Extension -> HSE.Extension
cabalExtensionToHSEExtension (Cabal.EnableExtension e) =
  HSE.EnableExtension $ fromMaybe HSE.ImplicitPrelude $ readMaybe $ show e
cabalExtensionToHSEExtension (Cabal.DisableExtension e) =
  HSE.DisableExtension $ fromMaybe HSE.ImplicitPrelude $ readMaybe $ show e
cabalExtensionToHSEExtension (Cabal.UnknownExtension e) = HSE.UnknownExtension e

hseExtensionToCabalExtension :: HSE.Extension -> Cabal.Extension
hseExtensionToCabalExtension (HSE.EnableExtension e) =
  Cabal.EnableExtension $ fromMaybe Cabal.ImplicitPrelude $ readMaybe $ show e
hseExtensionToCabalExtension (HSE.DisableExtension e) =
  Cabal.DisableExtension $ fromMaybe Cabal.ImplicitPrelude $ readMaybe $ show e
hseExtensionToCabalExtension (HSE.UnknownExtension e) = Cabal.UnknownExtension e

data SrcSpan =
  SrcSpan
    { srcSpanFilename    :: String
    , srcSpanStartLine   :: Int
    , srcSpanStartColumn :: Int
    , srcSpanEndLine     :: Int
    , srcSpanEndColumn   :: Int
    }
  deriving (Show, Eq)

fromHSESrcSpan :: HSE.SrcSpan -> SrcSpan
fromHSESrcSpan (HSE.SrcSpan name sl sc el ec) = SrcSpan name sl sc el ec

toHSESrcSpan :: SrcSpan -> HSE.SrcSpan
toHSESrcSpan (SrcSpan name sl sc el ec) = HSE.SrcSpan name sl sc el ec

newtype SrcSpanInfo =
  SrcSpanInfo
    { srcInfoSpan :: HSE.SrcSpan
    }

fromHSESrcSpanInfo :: HSE.SrcSpanInfo -> SrcSpanInfo
fromHSESrcSpanInfo (HSE.SrcSpanInfo s _) = SrcSpanInfo s

toExtension :: GLP.Extension -> HSE.Extension
toExtension = HSE.EnableExtension . read . show

gleExtensionToCabalExtension :: GLP.Extension -> Cabal.Extension
gleExtensionToCabalExtension = read . show

uniqueExtensions :: [Cabal.Extension] -> [GLP.Extension]
uniqueExtensions [] = []
uniqueExtensions ((Cabal.EnableExtension e):xs) =
  convertExtension e : uniqueExtensions xs
uniqueExtensions ((Cabal.DisableExtension e):xs) =
  uniqueExtensions $ filter (/= read (show e)) xs
uniqueExtensions ((Cabal.UnknownExtension s):_) =
  error $ "Unknown extension: " ++ s

convertComment :: GLP.LEpaComment -> Maybe HSE.Comment
convertComment (GLP.L anchor (GLP.EpaComment token _)) =
    case token of
        GLP.EpaEofComment -> Nothing
        GLP.EpaBlockComment comment -> Just $ HSE.Comment True (convertAnchor anchor) comment
        GLP.EpaLineComment comment -> Just $ HSE.Comment False (convertAnchor anchor) comment
        _ -> Nothing    -- Only these above comments appear.

convertAnchor :: GLP.Anchor -> HSE.SrcSpan
convertAnchor (GLP.Anchor anchor _) =
  HSE.SrcSpan
    (GLP.unpackFS $ GLP.srcSpanFile anchor)
    (GLP.srcSpanStartLine anchor)
    (GLP.srcSpanStartCol anchor)
    (GLP.srcSpanEndLine anchor)
    (GLP.srcSpanEndCol anchor)

-- `ghc-lib-parser`'s `Extension` does not implement `read`.
convertExtension :: Cabal.KnownExtension -> GLP.Extension
convertExtension Cabal.OverlappingInstances = GLP.OverlappingInstances
convertExtension Cabal.UndecidableInstances = GLP.UndecidableInstances
convertExtension Cabal.IncoherentInstances = GLP.IncoherentInstances
convertExtension Cabal.UndecidableSuperClasses = GLP.UndecidableSuperClasses
convertExtension Cabal.MonomorphismRestriction = GLP.MonomorphismRestriction
convertExtension Cabal.MonoLocalBinds = GLP.MonoLocalBinds
convertExtension Cabal.RelaxedPolyRec = GLP.RelaxedPolyRec
convertExtension Cabal.ExtendedDefaultRules = GLP.ExtendedDefaultRules
convertExtension Cabal.ForeignFunctionInterface = GLP.ForeignFunctionInterface
convertExtension Cabal.UnliftedFFITypes = GLP.UnliftedFFITypes
convertExtension Cabal.InterruptibleFFI = GLP.InterruptibleFFI
convertExtension Cabal.CApiFFI = GLP.CApiFFI
convertExtension Cabal.GHCForeignImportPrim = GLP.GHCForeignImportPrim
convertExtension Cabal.JavaScriptFFI = GLP.JavaScriptFFI
convertExtension Cabal.ParallelArrays = GLP.ParallelArrays
convertExtension Cabal.Arrows = GLP.Arrows
convertExtension Cabal.TemplateHaskell = GLP.TemplateHaskell
convertExtension Cabal.TemplateHaskellQuotes = GLP.TemplateHaskellQuotes
convertExtension Cabal.QualifiedDo = GLP.QualifiedDo
convertExtension Cabal.QuasiQuotes = GLP.QuasiQuotes
convertExtension Cabal.ImplicitParams = GLP.ImplicitParams
convertExtension Cabal.ImplicitPrelude = GLP.ImplicitPrelude
convertExtension Cabal.ScopedTypeVariables = GLP.ScopedTypeVariables
convertExtension Cabal.AllowAmbiguousTypes = GLP.AllowAmbiguousTypes
convertExtension Cabal.UnboxedTuples = GLP.UnboxedTuples
convertExtension Cabal.UnboxedSums = GLP.UnboxedSums
convertExtension Cabal.UnliftedNewtypes = GLP.UnliftedNewtypes
convertExtension Cabal.UnliftedDatatypes = GLP.UnliftedDatatypes
convertExtension Cabal.BangPatterns = GLP.BangPatterns
convertExtension Cabal.TypeFamilies = GLP.TypeFamilies
convertExtension Cabal.TypeFamilyDependencies = GLP.TypeFamilyDependencies
convertExtension Cabal.TypeInType = GLP.TypeInType
convertExtension Cabal.OverloadedStrings = GLP.OverloadedStrings
convertExtension Cabal.OverloadedLists = GLP.OverloadedLists
convertExtension Cabal.NumDecimals = GLP.NumDecimals
convertExtension Cabal.DisambiguateRecordFields = GLP.DisambiguateRecordFields
convertExtension Cabal.RecordWildCards = GLP.RecordWildCards
convertExtension Cabal.RecordPuns = GLP.RecordPuns
convertExtension Cabal.ViewPatterns = GLP.ViewPatterns
convertExtension Cabal.GADTs = GLP.GADTs
convertExtension Cabal.GADTSyntax = GLP.GADTSyntax
convertExtension Cabal.NPlusKPatterns = GLP.NPlusKPatterns
convertExtension Cabal.DoAndIfThenElse = GLP.DoAndIfThenElse
convertExtension Cabal.BlockArguments = GLP.BlockArguments
convertExtension Cabal.RebindableSyntax = GLP.RebindableSyntax
convertExtension Cabal.ConstraintKinds = GLP.ConstraintKinds
convertExtension Cabal.PolyKinds = GLP.PolyKinds
convertExtension Cabal.DataKinds = GLP.DataKinds
convertExtension Cabal.InstanceSigs = GLP.InstanceSigs
convertExtension Cabal.ApplicativeDo = GLP.ApplicativeDo
convertExtension Cabal.LinearTypes = GLP.LinearTypes
convertExtension Cabal.StandaloneDeriving = GLP.StandaloneDeriving
convertExtension Cabal.DeriveDataTypeable = GLP.DeriveDataTypeable
convertExtension Cabal.AutoDeriveTypeable = GLP.AutoDeriveTypeable
convertExtension Cabal.DeriveFunctor = GLP.DeriveFunctor
convertExtension Cabal.DeriveTraversable = GLP.DeriveTraversable
convertExtension Cabal.DeriveFoldable = GLP.DeriveFoldable
convertExtension Cabal.DeriveGeneric = GLP.DeriveGeneric
convertExtension Cabal.DefaultSignatures = GLP.DefaultSignatures
convertExtension Cabal.DeriveAnyClass = GLP.DeriveAnyClass
convertExtension Cabal.DeriveLift = GLP.DeriveLift
convertExtension Cabal.DerivingStrategies = GLP.DerivingStrategies
convertExtension Cabal.DerivingVia = GLP.DerivingVia
convertExtension Cabal.TypeSynonymInstances = GLP.TypeSynonymInstances
convertExtension Cabal.FlexibleContexts = GLP.FlexibleContexts
convertExtension Cabal.FlexibleInstances = GLP.FlexibleInstances
convertExtension Cabal.ConstrainedClassMethods = GLP.ConstrainedClassMethods
convertExtension Cabal.MultiParamTypeClasses = GLP.MultiParamTypeClasses
convertExtension Cabal.NullaryTypeClasses = GLP.NullaryTypeClasses
convertExtension Cabal.FunctionalDependencies = GLP.FunctionalDependencies
convertExtension Cabal.UnicodeSyntax = GLP.UnicodeSyntax
convertExtension Cabal.ExistentialQuantification = GLP.ExistentialQuantification
convertExtension Cabal.MagicHash = GLP.MagicHash
convertExtension Cabal.EmptyDataDecls = GLP.EmptyDataDecls
convertExtension Cabal.KindSignatures = GLP.KindSignatures
convertExtension Cabal.RoleAnnotations = GLP.RoleAnnotations
convertExtension Cabal.ParallelListComp = GLP.ParallelListComp
convertExtension Cabal.TransformListComp = GLP.TransformListComp
convertExtension Cabal.MonadComprehensions = GLP.MonadComprehensions
convertExtension Cabal.GeneralizedNewtypeDeriving =
  GLP.GeneralizedNewtypeDeriving
convertExtension Cabal.RecursiveDo = GLP.RecursiveDo
convertExtension Cabal.PostfixOperators = GLP.PostfixOperators
convertExtension Cabal.TupleSections = GLP.TupleSections
convertExtension Cabal.PatternGuards = GLP.PatternGuards
convertExtension Cabal.LiberalTypeSynonyms = GLP.LiberalTypeSynonyms
convertExtension Cabal.RankNTypes = GLP.RankNTypes
convertExtension Cabal.ImpredicativeTypes = GLP.ImpredicativeTypes
convertExtension Cabal.TypeOperators = GLP.TypeOperators
convertExtension Cabal.ExplicitNamespaces = GLP.ExplicitNamespaces
convertExtension Cabal.PackageImports = GLP.PackageImports
convertExtension Cabal.ExplicitForAll = GLP.ExplicitForAll
convertExtension Cabal.DatatypeContexts = GLP.DatatypeContexts
convertExtension Cabal.NondecreasingIndentation = GLP.NondecreasingIndentation
convertExtension Cabal.TraditionalRecordSyntax = GLP.TraditionalRecordSyntax
convertExtension Cabal.LambdaCase = GLP.LambdaCase
convertExtension Cabal.MultiWayIf = GLP.MultiWayIf
convertExtension Cabal.BinaryLiterals = GLP.BinaryLiterals
convertExtension Cabal.NegativeLiterals = GLP.NegativeLiterals
convertExtension Cabal.HexFloatLiterals = GLP.HexFloatLiterals
convertExtension Cabal.DuplicateRecordFields = GLP.DuplicateRecordFields
convertExtension Cabal.OverloadedLabels = GLP.OverloadedLabels
convertExtension Cabal.EmptyCase = GLP.EmptyCase
convertExtension Cabal.PatternSynonyms = GLP.PatternSynonyms
convertExtension Cabal.PartialTypeSignatures = GLP.PartialTypeSignatures
convertExtension Cabal.NamedWildCards = GLP.NamedWildCards
convertExtension Cabal.StaticPointers = GLP.StaticPointers
convertExtension Cabal.TypeApplications = GLP.TypeApplications
convertExtension Cabal.Strict = GLP.Strict
convertExtension Cabal.StrictData = GLP.StrictData
convertExtension Cabal.EmptyDataDeriving = GLP.EmptyDataDeriving
convertExtension Cabal.NumericUnderscores = GLP.NumericUnderscores
convertExtension Cabal.QuantifiedConstraints = GLP.QuantifiedConstraints
convertExtension Cabal.StarIsType = GLP.StarIsType
convertExtension Cabal.ImportQualifiedPost = GLP.ImportQualifiedPost
convertExtension Cabal.CUSKs = GLP.CUSKs
convertExtension Cabal.StandaloneKindSignatures = GLP.StandaloneKindSignatures
convertExtension Cabal.LexicalNegation = GLP.LexicalNegation
convertExtension Cabal.FieldSelectors = GLP.FieldSelectors
convertExtension Cabal.OverloadedRecordDot = GLP.OverloadedRecordDot
convertExtension e =
  error $ "This extension is not supported by `ghc-lib-parser`: " ++ show e
