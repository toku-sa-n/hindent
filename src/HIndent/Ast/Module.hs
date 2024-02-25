-- | Module type.
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ViewPatterns     #-}

module HIndent.Ast.Module
  ( Module(..)
  , mkModule
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Maybe
import           GHC.Hs                        hiding (comments)
import qualified GHC.Hs                        as GHC
import           GHC.Types.SrcLoc
import           HIndent.Applicative
import           HIndent.Ast.ModuleDeclaration
import           HIndent.Ast.Pragma
import           HIndent.Ast.WithComments
import           HIndent.Config
import           HIndent.Pretty
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Import
import           HIndent.Pretty.NodeComments
import           HIndent.Pretty.Types
import           HIndent.Printer
#if MIN_VERSION_ghc_lib_parser(9,6,1)
import           GHC.Core.DataCon
#endif

#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GHC.GhcPs
#else
type HsModule' = HsModule
#endif
data Module = Module
  { pragmas     :: [Pragma]
  , declaration :: Maybe ModuleDeclaration
  , module'     :: HsModule'
  }

instance CommentExtraction Module where
  nodeComments (Module {..}) = nodeComments module'
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
instance Pretty Module where
  pretty' m@Module { declaration = Nothing
                   , module' = HsModule {hsmodImports = [], hsmodDecls = []}
                   }
    | not (pragmaExists m) = pure ()
  pretty' mo@Module {module' = m} = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists mo, prettyPragmas mo)
        , (moduleDeclExists mo, prettyModuleDecl m)
        , (importsExist m, prettyImports)
        , (declsExist m, prettyDecls)
        ]
      prettyModuleDecl HsModule {hsmodName = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Nothing
                                , hsmodExt = XModulePs {..}
                                , ..
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        whenJust hsmodDeprecMessage $ \x -> do
          space
          pretty $ fmap ModuleDeprecatedPragma x
        string " where"
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Just exports
                                , hsmodExt = XModulePs {..}
                                , ..
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        whenJust hsmodDeprecMessage $ \x -> do
          space
          pretty $ fmap ModuleDeprecatedPragma x
        newline
        indentedBlock $ do
          printCommentsAnd exports (vTuple . fmap pretty)
          string " where"
      moduleDeclExists = isJust . declaration
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addDeclSeparator $ hsmodDecls m
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {})   = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator (SigD _ PatSynSig {}) = newline
      declSeparator _                     = blankline
      declsExist = not . null . hsmodDecls
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True  -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
#else
instance Pretty Module where
  pretty' m@Module { declaration = Nothing
                   , module' = HsModule {hsmodImports = [], hsmodDecls = []}
                   }
    | not (pragmaExists m) = pure ()
  pretty' mo@Module {module' = m} = blanklined printers >> newline
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists mo, prettyPragmas mo)
        , (moduleDeclExists mo, prettyModuleDecl m)
        , (importsExist m, prettyImports)
        , (declsExist m, prettyDecls)
        ]
      prettyModuleDecl HsModule {hsmodName = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Nothing
                                , ..
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        whenJust hsmodDeprecMessage $ \x -> do
          space
          pretty $ fmap ModuleDeprecatedPragma x
        string " where"
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Just exports
                                , ..
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        whenJust hsmodDeprecMessage $ \x -> do
          space
          pretty $ fmap ModuleDeprecatedPragma x
        newline
        indentedBlock $ do
          printCommentsAnd exports (vTuple . fmap pretty)
          string " where"
      moduleDeclExists = isJust . declaration
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addDeclSeparator $ hsmodDecls m
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {})   = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator (SigD _ PatSynSig {}) = newline
      declSeparator _                     = blankline
      declsExist = not . null . hsmodDecls
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True  -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
#endif
mkModule :: HsModule' -> WithComments Module
mkModule m =
  WithComments
    { comments = epas m
    , node =
        Module
          { pragmas = mkPragmas m
          , declaration = mkModuleDeclaration m
          , module' = m
          }
    }
  where
    epas = epaComments . filterOutEofAndPragmasFromAnn . getAnn
      where
        filterOutEofAndPragmasFromAnn EpAnn {..} =
          EpAnn {comments = filterOutEofAndPragmasFromComments comments, ..}
        filterOutEofAndPragmasFromAnn EpAnnNotUsed = EpAnnNotUsed
        filterOutEofAndPragmasFromComments comments =
          EpaCommentsBalanced
            { priorComments = filterOutEofAndPragmas $ priorComments comments
            , followingComments =
                filterOutEofAndPragmas $ getFollowingComments comments
            }
        filterOutEofAndPragmas = filter isNeitherEofNorPragmaComment
        isNeitherEofNorPragmaComment (L _ (EpaComment EpaEofComment _)) = False
        isNeitherEofNorPragmaComment (L _ (EpaComment tok _)) =
          not $ isPragma tok

getAnn :: HsModule' -> EpAnn GHC.AnnsModule
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getAnn = hsmodAnn . hsmodExt
#else
getAnn = hsmodAnn
#endif
epaComments :: EpAnn a -> NodeComments
epaComments (EpAnn ann _ cs) = NodeComments {..}
  where
    commentsBefore = priorComments cs
    commentsOnSameLine = filter isCommentOnSameLine $ getFollowingComments cs
    commentsAfter = filter (not . isCommentOnSameLine) $ getFollowingComments cs
    isCommentOnSameLine (L comAnn _) =
      srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
epaComments EpAnnNotUsed = NodeComments [] [] []

printCommentsAnd ::
     (CommentExtraction l) => GenLocated l e -> (e -> Printer ()) -> Printer ()
printCommentsAnd (L l e) f = do
  printCommentsBefore l
  f e
  printCommentOnSameLine l
  printCommentsAfter l

-- | Prints comments that are before the given AST node.
printCommentsBefore :: CommentExtraction a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore $ nodeComments p) $ \(L loc c) -> do
    let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints comments that are on the same line as the given AST node.
printCommentOnSameLine :: CommentExtraction a => a -> Printer ()
printCommentOnSameLine (commentsOnSameLine . nodeComments -> (c:cs)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel
           (fromIntegral $ srcSpanStartCol $ anchor $ getLoc c) $
         spaced $ fmap pretty $ c : cs
    else spacePrefixed $ fmap pretty $ c : cs
  eolCommentsArePrinted
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: CommentExtraction a => a -> Printer ()
printCommentsAfter p =
  case commentsAfter $ nodeComments p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(L loc c) -> do
        let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted

pragmaExists :: Module -> Bool
pragmaExists = not . null . pragmas

prettyPragmas :: Module -> Printer ()
prettyPragmas = lined . fmap pretty . pragmas
