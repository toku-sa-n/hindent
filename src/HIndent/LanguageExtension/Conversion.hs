{-# LANGUAGE CPP #-}

-- | Operations for converting extensions types.
module HIndent.LanguageExtension.Conversion
  ( readCabalExtension
  , glpExtensionToCabalExtension
  , uniqueExtensions
  , convertExtension
  ) where

import qualified GHC.LanguageExtensions     as GLP
import           HIndent.Read
import qualified Language.Haskell.Extension as Cabal
import           Text.Read

readCabalExtension :: String -> Maybe Cabal.Extension
readCabalExtension ('N':'o':xs) = Cabal.DisableExtension <$> readMaybe xs
readCabalExtension xs           = Cabal.EnableExtension <$> readMaybe xs

-- | Converts a value of the type 'Extension' defined in the
-- 'ghc-lib-parser' package to the same value of the type 'Extension'
-- defined in the 'Cabal' package.
--
-- If 'Cabal' does not support the given extension, it returns a 'Nothing'.
glpExtensionToCabalExtension :: GLP.Extension -> Maybe Cabal.Extension
glpExtensionToCabalExtension = fmap Cabal.EnableExtension . readMaybe . show

-- | This function converts each value of the type 'Extension' defined in
-- the package 'Cabal' in the list to the same value of the type
-- 'Extension' defined in the package 'ghc-lib-parser'.
--
-- If the extension has the 'No' suffix, the extension is removed from the
-- result. If both extensions having and not having the suffix exist in the
-- list, only the most backward one has the effect.
--
-- If converting an extension fails due to neither GHC nor 'ghc-lib-parser'
-- not supporting, or deprecation or removal, the extension is ignored.
uniqueExtensions :: [Cabal.Extension] -> [GLP.Extension]
uniqueExtensions [] = []
uniqueExtensions ((Cabal.EnableExtension e):xs)
  | Just e' <- convertExtension e = e' : uniqueExtensions xs
  | otherwise = uniqueExtensions xs
uniqueExtensions ((Cabal.DisableExtension e):xs) =
  uniqueExtensions $ filter (/= readOrFail (show $ Cabal.EnableExtension e)) xs
uniqueExtensions ((Cabal.UnknownExtension s):_) =
  error $ "Unknown extension: " ++ s

-- | This function converts a value of 'KnownExtension' defined in the
-- 'Cabal' package to the same value of 'Extension' defined in
-- 'ghc-lib-parser'.
--
-- The implementation of this function is very long because 'Extension'
-- does not implement 'Read'.
--
-- This function returns a 'Just' value if it succeeds in converting.
-- Otherwise (e.g., neigher GHC nor 'ghc-lib-parser' does not the passed
-- extension, or it is deprecated or removed), it returns a 'Nothing'.
convertExtension :: Cabal.KnownExtension -> Maybe GLP.Extension
convertExtension Cabal.OverlappingInstances = Just GLP.OverlappingInstances
convertExtension Cabal.UndecidableInstances = Just GLP.UndecidableInstances
convertExtension Cabal.IncoherentInstances = Just GLP.IncoherentInstances
convertExtension Cabal.UndecidableSuperClasses =
  Just GLP.UndecidableSuperClasses
convertExtension Cabal.MonomorphismRestriction =
  Just GLP.MonomorphismRestriction
convertExtension Cabal.MonoLocalBinds = Just GLP.MonoLocalBinds
convertExtension Cabal.RelaxedPolyRec = Just GLP.RelaxedPolyRec
convertExtension Cabal.ExtendedDefaultRules = Just GLP.ExtendedDefaultRules
convertExtension Cabal.ForeignFunctionInterface =
  Just GLP.ForeignFunctionInterface
convertExtension Cabal.UnliftedFFITypes = Just GLP.UnliftedFFITypes
convertExtension Cabal.InterruptibleFFI = Just GLP.InterruptibleFFI
convertExtension Cabal.CApiFFI = Just GLP.CApiFFI
convertExtension Cabal.GHCForeignImportPrim = Just GLP.GHCForeignImportPrim
convertExtension Cabal.JavaScriptFFI = Just GLP.JavaScriptFFI
convertExtension Cabal.ParallelArrays = Just GLP.ParallelArrays
convertExtension Cabal.Arrows = Just GLP.Arrows
convertExtension Cabal.TemplateHaskell = Just GLP.TemplateHaskell
convertExtension Cabal.TemplateHaskellQuotes = Just GLP.TemplateHaskellQuotes
convertExtension Cabal.QuasiQuotes = Just GLP.QuasiQuotes
convertExtension Cabal.ImplicitParams = Just GLP.ImplicitParams
convertExtension Cabal.ImplicitPrelude = Just GLP.ImplicitPrelude
convertExtension Cabal.ScopedTypeVariables = Just GLP.ScopedTypeVariables
convertExtension Cabal.AllowAmbiguousTypes = Just GLP.AllowAmbiguousTypes
convertExtension Cabal.UnboxedTuples = Just GLP.UnboxedTuples
convertExtension Cabal.UnboxedSums = Just GLP.UnboxedSums
convertExtension Cabal.UnliftedNewtypes = Just GLP.UnliftedNewtypes
convertExtension Cabal.BangPatterns = Just GLP.BangPatterns
convertExtension Cabal.TypeFamilies = Just GLP.TypeFamilies
convertExtension Cabal.TypeFamilyDependencies = Just GLP.TypeFamilyDependencies
convertExtension Cabal.TypeInType = Just GLP.TypeInType
convertExtension Cabal.OverloadedStrings = Just GLP.OverloadedStrings
convertExtension Cabal.OverloadedLists = Just GLP.OverloadedLists
convertExtension Cabal.NumDecimals = Just GLP.NumDecimals
convertExtension Cabal.DisambiguateRecordFields =
  Just GLP.DisambiguateRecordFields
convertExtension Cabal.RecordWildCards = Just GLP.RecordWildCards
convertExtension Cabal.ViewPatterns = Just GLP.ViewPatterns
convertExtension Cabal.GADTs = Just GLP.GADTs
convertExtension Cabal.GADTSyntax = Just GLP.GADTSyntax
convertExtension Cabal.NPlusKPatterns = Just GLP.NPlusKPatterns
convertExtension Cabal.DoAndIfThenElse = Just GLP.DoAndIfThenElse
convertExtension Cabal.BlockArguments = Just GLP.BlockArguments
convertExtension Cabal.RebindableSyntax = Just GLP.RebindableSyntax
convertExtension Cabal.ConstraintKinds = Just GLP.ConstraintKinds
convertExtension Cabal.PolyKinds = Just GLP.PolyKinds
convertExtension Cabal.DataKinds = Just GLP.DataKinds
convertExtension Cabal.InstanceSigs = Just GLP.InstanceSigs
convertExtension Cabal.ApplicativeDo = Just GLP.ApplicativeDo
convertExtension Cabal.StandaloneDeriving = Just GLP.StandaloneDeriving
convertExtension Cabal.DeriveDataTypeable = Just GLP.DeriveDataTypeable
convertExtension Cabal.AutoDeriveTypeable = Just GLP.AutoDeriveTypeable
convertExtension Cabal.DeriveFunctor = Just GLP.DeriveFunctor
convertExtension Cabal.DeriveTraversable = Just GLP.DeriveTraversable
convertExtension Cabal.DeriveFoldable = Just GLP.DeriveFoldable
convertExtension Cabal.DeriveGeneric = Just GLP.DeriveGeneric
convertExtension Cabal.DefaultSignatures = Just GLP.DefaultSignatures
convertExtension Cabal.DeriveAnyClass = Just GLP.DeriveAnyClass
convertExtension Cabal.DeriveLift = Just GLP.DeriveLift
convertExtension Cabal.DerivingStrategies = Just GLP.DerivingStrategies
convertExtension Cabal.DerivingVia = Just GLP.DerivingVia
convertExtension Cabal.TypeSynonymInstances = Just GLP.TypeSynonymInstances
convertExtension Cabal.FlexibleContexts = Just GLP.FlexibleContexts
convertExtension Cabal.FlexibleInstances = Just GLP.FlexibleInstances
convertExtension Cabal.ConstrainedClassMethods =
  Just GLP.ConstrainedClassMethods
convertExtension Cabal.MultiParamTypeClasses = Just GLP.MultiParamTypeClasses
convertExtension Cabal.NullaryTypeClasses = Just GLP.NullaryTypeClasses
convertExtension Cabal.FunctionalDependencies = Just GLP.FunctionalDependencies
convertExtension Cabal.UnicodeSyntax = Just GLP.UnicodeSyntax
convertExtension Cabal.ExistentialQuantification =
  Just GLP.ExistentialQuantification
convertExtension Cabal.MagicHash = Just GLP.MagicHash
convertExtension Cabal.EmptyDataDecls = Just GLP.EmptyDataDecls
convertExtension Cabal.KindSignatures = Just GLP.KindSignatures
convertExtension Cabal.RoleAnnotations = Just GLP.RoleAnnotations
convertExtension Cabal.ParallelListComp = Just GLP.ParallelListComp
convertExtension Cabal.TransformListComp = Just GLP.TransformListComp
convertExtension Cabal.MonadComprehensions = Just GLP.MonadComprehensions
convertExtension Cabal.GeneralizedNewtypeDeriving =
  Just GLP.GeneralizedNewtypeDeriving
convertExtension Cabal.RecursiveDo = Just GLP.RecursiveDo
convertExtension Cabal.PostfixOperators = Just GLP.PostfixOperators
convertExtension Cabal.TupleSections = Just GLP.TupleSections
convertExtension Cabal.PatternGuards = Just GLP.PatternGuards
convertExtension Cabal.LiberalTypeSynonyms = Just GLP.LiberalTypeSynonyms
convertExtension Cabal.RankNTypes = Just GLP.RankNTypes
convertExtension Cabal.ImpredicativeTypes = Just GLP.ImpredicativeTypes
convertExtension Cabal.TypeOperators = Just GLP.TypeOperators
convertExtension Cabal.ExplicitNamespaces = Just GLP.ExplicitNamespaces
convertExtension Cabal.PackageImports = Just GLP.PackageImports
convertExtension Cabal.ExplicitForAll = Just GLP.ExplicitForAll
convertExtension Cabal.DatatypeContexts = Just GLP.DatatypeContexts
convertExtension Cabal.NondecreasingIndentation =
  Just GLP.NondecreasingIndentation
convertExtension Cabal.TraditionalRecordSyntax =
  Just GLP.TraditionalRecordSyntax
convertExtension Cabal.LambdaCase = Just GLP.LambdaCase
convertExtension Cabal.MultiWayIf = Just GLP.MultiWayIf
convertExtension Cabal.BinaryLiterals = Just GLP.BinaryLiterals
convertExtension Cabal.NegativeLiterals = Just GLP.NegativeLiterals
convertExtension Cabal.HexFloatLiterals = Just GLP.HexFloatLiterals
convertExtension Cabal.DuplicateRecordFields = Just GLP.DuplicateRecordFields
convertExtension Cabal.OverloadedLabels = Just GLP.OverloadedLabels
convertExtension Cabal.EmptyCase = Just GLP.EmptyCase
convertExtension Cabal.PatternSynonyms = Just GLP.PatternSynonyms
convertExtension Cabal.PartialTypeSignatures = Just GLP.PartialTypeSignatures
convertExtension Cabal.NamedWildCards = Just GLP.NamedWildCards
convertExtension Cabal.StaticPointers = Just GLP.StaticPointers
convertExtension Cabal.TypeApplications = Just GLP.TypeApplications
convertExtension Cabal.Strict = Just GLP.Strict
convertExtension Cabal.StrictData = Just GLP.StrictData
convertExtension Cabal.EmptyDataDeriving = Just GLP.EmptyDataDeriving
convertExtension Cabal.NumericUnderscores = Just GLP.NumericUnderscores
convertExtension Cabal.QuantifiedConstraints = Just GLP.QuantifiedConstraints
convertExtension Cabal.StarIsType = Just GLP.StarIsType
convertExtension Cabal.ImportQualifiedPost = Just GLP.ImportQualifiedPost
convertExtension Cabal.CUSKs = Just GLP.CUSKs
convertExtension Cabal.StandaloneKindSignatures =
  Just GLP.StandaloneKindSignatures
convertExtension Cabal.DoRec = Just GLP.RecursiveDo -- 'DoRec' is deprecated.
convertExtension Cabal.GeneralisedNewtypeDeriving =
  Just GLP.GeneralizedNewtypeDeriving
convertExtension Cabal.PatternSignatures = Just GLP.ScopedTypeVariables
convertExtension Cabal.CPP = Just GLP.Cpp
convertExtension Cabal.Rank2Types = Just GLP.RankNTypes -- See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/rank_polymorphism.html.
convertExtension Cabal.PolymorphicComponents = Just GLP.RankNTypes -- See https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/exts/rank_polymorphism.html.
convertExtension Cabal.Generics = Nothing -- This extension is no longer supported.
convertExtension Cabal.ExtensibleRecords = Nothing -- This extension is supported by Hugs, but not by GHC.
convertExtension Cabal.RestrictedTypeSynonyms = Nothing -- This extension is supported by Hugs, but not by GHC.
convertExtension Cabal.HereDocuments = Nothing -- This extension is supported by Hugs, but not by GHC.
convertExtension Cabal.MonoPatBinds = Nothing -- This extension has no effect. See https://hackage.haskell.org/package/Cabal-3.6.3.0/docs/Language-Haskell-Extension.html#t:Extension.
convertExtension Cabal.NewQualifiedOperators = Nothing -- This extension is deprecated.
convertExtension Cabal.XmlSyntax = Nothing -- This extension is not supported by GHC.
convertExtension Cabal.RegularPatterns = Nothing -- This extension is not supported by GHC.
convertExtension Cabal.SafeImports = Nothing -- This extension is not supported by 'ghc-lib-parser'.
convertExtension Cabal.Safe = Nothing -- This extension is not supported by 'ghc-lib-parser'.
convertExtension Cabal.Trustworthy = Nothing -- This extension is not supported by 'ghc-lib-parser'.
convertExtension Cabal.Unsafe = Nothing -- This extension is not supported by 'ghc-lib-parser'.
convertExtension Cabal.MonadFailDesugaring = Nothing -- This extension is enabled by default.
#if MIN_VERSION_Cabal(3,6,0)
convertExtension Cabal.OverloadedRecordDot = Just GLP.OverloadedRecordDot
convertExtension Cabal.FieldSelectors = Just GLP.FieldSelectors
convertExtension Cabal.LexicalNegation = Just GLP.LexicalNegation
convertExtension Cabal.LinearTypes = Just GLP.LinearTypes
convertExtension Cabal.UnliftedDatatypes = Just GLP.UnliftedDatatypes
convertExtension Cabal.QualifiedDo = Just GLP.QualifiedDo
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
convertExtension Cabal.NamedFieldPuns = Just GLP.NamedFieldPuns
convertExtension Cabal.OverloadedRecordUpdate = Just GLP.OverloadedRecordUpdate
convertExtension Cabal.AlternativeLayoutRule = Just GLP.AlternativeLayoutRule
convertExtension Cabal.AlternativeLayoutRuleTransitional =
  Just GLP.AlternativeLayoutRuleTransitional
convertExtension Cabal.RelaxedLayout = Just GLP.RelaxedLayout
convertExtension Cabal.RecordPuns = Nothing -- This extension is incorporated in GHC2021.
#else
convertExtension Cabal.RecordPuns = Just GLP.RecordPuns
convertExtension Cabal.NamedFieldPuns = Nothing -- This extension is not supported by 'ghc-lib-parser'.
#endif
