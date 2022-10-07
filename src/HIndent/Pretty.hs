{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Pretty printing.
--
-- We define new types to pretty-print AST nodes rather than define
-- functions to print comments easily using the 'Pretty' implementation of
-- 'GenLocated'. However, some instances define top-level functions to
-- handle CPP.
module HIndent.Pretty
  ( pretty
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.List
import           Data.Maybe
import           Data.Void
import           Generics.SYB                 hiding (Fixity, Infix, Prefix)
import           GHC.Core.Coercion
import           GHC.Core.InstEnv
import           GHC.Data.Bag
import           GHC.Data.BooleanFormula
import           GHC.Data.FastString
import           GHC.Hs
import           GHC.Types.Basic
import           GHC.Types.Fixity
import           GHC.Types.ForeignCall
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           GHC.Types.SourceText
import           GHC.Types.SrcLoc
import           GHC.Types.Var
import           GHC.Unit
import           GHC.Unit.Module.Warnings
import           HIndent.Applicative
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Imports.Sort
import           HIndent.Pretty.Pragma
import           HIndent.Pretty.SigBindFamily
import           HIndent.Types
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import           GHC.Types.PkgQual
#endif
newtype InfixExpr =
  InfixExpr (LHsExpr GhcPs)

newtype InfixOp =
  InfixOp RdrName

-- | A wrapper type for printing an identifier as a prefix operator.
--
-- Printing a `PrefixOp` value containing a symbol operator wraps it with
-- parentheses.
newtype PrefixOp =
  PrefixOp RdrName

data InfixApp =
  InfixApp
    { lhs                :: LHsExpr GhcPs
    , op                 :: LHsExpr GhcPs
    , rhs                :: LHsExpr GhcPs
    , immediatelyAfterDo :: Bool
    }

newtype MatchGroupForCase =
  MatchGroupForCase (MatchGroup GhcPs (LHsExpr GhcPs))

newtype MatchGroupForLambda =
  MatchGroupForLambda (MatchGroup GhcPs (LHsExpr GhcPs))

newtype MatchForCase =
  MatchForCase (Match GhcPs (LHsExpr GhcPs))

newtype MatchForLambda =
  MatchForLambda (Match GhcPs (LHsExpr GhcPs))

newtype GRHSsForCase =
  GRHSsForCase (GRHSs GhcPs (LHsExpr GhcPs))

newtype GRHSsForLambda =
  GRHSsForLambda (GRHSs GhcPs (LHsExpr GhcPs))

newtype GRHSForCase =
  GRHSForCase (GRHS GhcPs (LHsExpr GhcPs))

newtype GRHSForMultiwayIf =
  GRHSForMultiwayIf (GRHS GhcPs (LHsExpr GhcPs))

newtype GRHSForLambda =
  GRHSForLambda (GRHS GhcPs (LHsExpr GhcPs))

newtype RecConPat =
  RecConPat (HsRecFields GhcPs (LPat GhcPs))
#if MIN_VERSION_ghc_lib_parser(9,4,1)
newtype RecConField =
  RecConField (HsFieldBind (LFieldOcc GhcPs) (LPat GhcPs))
#else
newtype RecConField =
  RecConField (HsRecField' (FieldOcc GhcPs) (LPat GhcPs))
#endif
newtype HsSigTypeInsideInstDecl =
  HsSigTypeInsideInstDecl (HsSigType GhcPs)

newtype HsSigTypeInsideVerticalFuncSig =
  HsSigTypeInsideVerticalFuncSig (HsSigType GhcPs)

newtype HsSigTypeInsideDeclSig =
  HsSigTypeInsideDeclSig (HsSigType GhcPs)

newtype HsTypeInsideInstDecl =
  HsTypeInsideInstDecl (HsType GhcPs)

newtype HsTypeInsideVerticalFuncSig =
  HsTypeInsideVerticalFuncSig (HsType GhcPs)

newtype StmtLRInsideVerticalList =
  StmtLRInsideVerticalList (StmtLR GhcPs GhcPs (LHsExpr GhcPs))

newtype ParStmtBlockInsideVerticalList =
  ParStmtBlockInsideVerticalList (ParStmtBlock GhcPs GhcPs)

newtype DeclSig =
  DeclSig (Sig GhcPs)

-- | Top-level type family declaration, like @type family ID a :: *@.
newtype DeclTypeFamily =
  DeclTypeFamily (FamilyDecl GhcPs)

newtype HsTypeInsideDeclSig =
  HsTypeInsideDeclSig (HsType GhcPs)

newtype HsTypeInsideVerticalDeclSig =
  HsTypeInsideVerticalDeclSig (HsType GhcPs)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | A wrapper type for type class constraints; e.g., (Eq a, Ord a) of (Eq
-- a, Ord a) => [a] -> [a]. Either 'HorizontalContext' or 'VerticalContext'
-- is used internally.
newtype Context =
  Context (LHsContext GhcPs)

-- | A wrapper type for printing a context horizontally.
newtype HorizontalContext =
  HorizontalContext (LHsContext GhcPs)

-- | A wrapper type for printing a context vertically.
newtype VerticalContext =
  VerticalContext (LHsContext GhcPs)
#else
-- | A wrapper type for type class constraints; e.g., (Eq a, Ord a) of (Eq
-- a, Ord a) => [a] -> [a]. Either 'HorizontalContext' or 'VerticalContext'
-- is used internally.
newtype Context =
  Context (Maybe (LHsContext GhcPs))

-- | A wrapper type for printing a context horizontally.
newtype HorizontalContext =
  HorizontalContext (Maybe (LHsContext GhcPs))

-- | A wrapper type for printing a context vertically.
newtype VerticalContext =
  VerticalContext (Maybe (LHsContext GhcPs))
#endif
-- | 'HsDataDefn' for 'data instance'.
newtype HsDataDefnForDataInstance =
  HsDataDefnForDataInstance (HsDataDefn GhcPs)

-- | A wrapper type for pretty-printing a value of @ModuleName@ with the
-- @module @ prefix.
--
-- Pretty-printing it via @(string "module " >> pretty (name ::
-- ModuleName))@ locates comments before @name@ in the same line as @module
-- @ and the name will be in the next line. This type is to avoid the
-- problem.
newtype ModuleNameWithPrefix =
  ModuleNameWithPrefix ModuleName

-- | This function pretty-prints the given AST node with comments.
pretty :: Pretty a => a -> Printer ()
pretty p = do
  printCommentsBefore p
  pretty' p
  printCommentOnSameLine p
  printCommentsAfter p

printCommentsAnd ::
     (Pretty l) => GenLocated l e -> (e -> Printer ()) -> Printer ()
printCommentsAnd (L l e) f = do
  printCommentsBefore l
  f e
  printCommentOnSameLine l
  printCommentsAfter l

-- | Prints comments that are before the given AST node.
printCommentsBefore :: Pretty a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore p) $ \(L loc c) -> do
    let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
    indentedWithFixedLevel col $ pretty c
    newline

-- | Prints a comment that is on the same line as the given AST node if it exists.
printCommentOnSameLine :: Pretty a => a -> Printer ()
printCommentOnSameLine (commentOnSameLine -> Just (L sp c)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithFixedLevel (fromIntegral $ srcSpanStartCol $ anchor sp) $
         pretty c
    else do
      space
      pretty c
  eolCommentsArePrinted
printCommentOnSameLine _ = return ()

-- | Prints comments that are after the given AST node.
printCommentsAfter :: Pretty a => a -> Printer ()
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(L loc c) -> do
        let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
        indentedWithFixedLevel col $ pretty c
        eolCommentsArePrinted

-- | Pretty print including comments.
--
-- FIXME: 'Pretty' has a problem. It has two responsibilities; one is to
-- print a given node pretty, and the other is to collect comments from the
-- node.
--
-- Note that there are three types of nodes:
-- * A node that can pretty-print and has comments (e.g., 'HsModule')
-- * A node that can pretty-print but has no comments (e.g., almost all
-- nodes)
-- * A node that cannot pretty-print but has comments (e.g., 'EpAnn')
class Pretty a where
  pretty' :: a -> Printer ()
  -- | Returns comments that are before the given AST node.
  commentsBefore :: a -> [LEpaComment]
  commentsBefore = const []
  -- | Returns a comment that is on the same line as the last line of the given AST node if it exists.
  commentOnSameLine :: a -> Maybe LEpaComment
  commentOnSameLine = const Nothing
  -- | Returns comments that are after the given AST node.
  commentsAfter :: a -> [LEpaComment]
  commentsAfter = const []

instance Pretty HsModule where
  pretty' m = do
    blanklined printers
    newline
    -- TODO: Refactor this 'where' clause.
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists m, prettyPragmas m)
        , (moduleDeclarationExists m, outputModuleDeclaration m)
        , (importsExist m, outputImports m)
        , (declsExist m, outputDecls)
        ]
      outputModuleDeclaration HsModule {hsmodName = Nothing} = return ()
      outputModuleDeclaration HsModule { hsmodName = Just name
                                       , hsmodExports = Nothing
                                       } = do
        pretty $ fmap ModuleNameWithPrefix name
        string " where"
      outputModuleDeclaration HsModule { hsmodName = Just name
                                       , hsmodExports = Just (L _ xs)
                                       } = do
        pretty $ fmap ModuleNameWithPrefix name
        newline
        indentedBlock $ do
          vTuple $ fmap pretty xs
          string " where"
      moduleDeclarationExists HsModule {hsmodName = Nothing} = False
      moduleDeclarationExists _                              = True
      outputDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addSeparator $ hsmodDecls m
      addSeparator []     = []
      addSeparator [x]    = [(x, Nothing)]
      addSeparator (x:xs) = (x, Just $ separator $ unLoc x) : addSeparator xs
      separator (SigD _ TypeSig {})   = newline
      separator (SigD _ InlineSig {}) = newline
      separator _                     = blankline
      declsExist = not . null . hsmodDecls
      -- TODO: Support 'configSortImports'.
      outputImports =
        blanklined .
        fmap (outputImportGroup . sortImportsByName) .
        groupImports . sortImportsByLocation . hsmodImports
      outputImportGroup = lined . fmap pretty
      importsExist = not . null . hsmodImports
      groupImports = groupImports' []
        where
          groupImports' ::
               [[LImportDecl GhcPs]]
            -> [LImportDecl GhcPs]
            -> [[LImportDecl GhcPs]]
          groupImports' xs [] = xs
          groupImports' [] (x:xs) = groupImports' [[x]] xs
          groupImports' [[]] (x:xs) = groupImports' [[x]] xs
          groupImports' ([]:x:xs) (y:ys) = groupImports' ([y] : x : xs) ys
          groupImports' ((z:zs):xs) (y:ys)
            | z `isAdjacentTo` y = groupImports' ((y : z : zs) : xs) ys
            | otherwise = groupImports' ([y] : (z : zs) : xs) ys
          a `isAdjacentTo` b =
            srcSpanEndLine (sp a) + 1 == srcSpanStartLine (sp b) ||
            srcSpanEndLine (sp b) + 1 == srcSpanStartLine (sp a)
          sp x =
            case locA $ getLoc x of
              RealSrcSpan x' _ -> x'
              _                -> error "Src span unavailable."
  commentsBefore =
    filter (not . isPragma . ac_tok . unLoc) .
    listify (not . isEofComment) . priorComments . comments . hsmodAnn
    where
      isEofComment (L _ (EpaComment EpaEofComment _)) = True
      isEofComment _                                  = False
  commentsAfter =
    filter (not . isPragma . ac_tok . unLoc) .
    followingComments . comments . hsmodAnn

-- FIXME: Requiring 'l' to implement 'Pretty' is wrong because some types
-- (e.g., 'EpAnn') cannot pretty-print. The restriction exists only for
-- extracting comments. Remove the restriction.
instance (Pretty l, Pretty e) => Pretty (GenLocated l e) where
  pretty' (L _ e) = pretty e
  commentsBefore (L l _) = commentsBefore l
  commentOnSameLine (L l _) = commentOnSameLine l
  commentsAfter (L l _) = commentsAfter l

instance Pretty (HsDecl GhcPs) where
  pretty' (TyClD _ d)      = pretty d
  pretty' (InstD _ inst)   = pretty inst
  pretty' (DerivD _ x)     = pretty x
  pretty' (ValD _ bind)    = pretty bind
  pretty' (SigD _ s)       = pretty $ DeclSig s
  pretty' (KindSigD _ x)   = pretty x
  pretty' (DefD _ x)       = pretty x
  pretty' (ForD _ x)       = pretty x
  pretty' (WarningD _ x)   = pretty x
  pretty' (AnnD _ x)       = pretty x
  pretty' (RuleD _ x)      = pretty x
  pretty' (SpliceD _ sp)   = pretty sp
  pretty' DocD {}          = return ()
  pretty' (RoleAnnotD _ x) = pretty x

instance Pretty (TyClDecl GhcPs) where
  pretty' = prettyTyClDecl

prettyTyClDecl :: TyClDecl GhcPs -> Printer ()
prettyTyClDecl (FamDecl _ x) = pretty $ DeclTypeFamily x
prettyTyClDecl SynDecl {..} = do
  string "type "
    -- TODO: Merge this case with the one in 'ClassDecl's branch.
  case tcdFixity of
    Prefix -> spaced $ pretty tcdLName : fmap pretty (hsq_explicit tcdTyVars)
    Infix ->
      case hsq_explicit tcdTyVars of
        (l:r:xs) -> do
          spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
          forM_ xs $ \x -> do
            space
            pretty x
        _ -> error "Not enough parameters are given."
  hor <-|> ver
  where
    hor = do
      string " = "
      pretty tcdRhs
    ver =
      indentedWithSpace 3 $ do
        newline
        string "= "
        indentedBlock $ pretty tcdRhs
prettyTyClDecl DataDecl {..} = do
  case dd_ND tcdDataDefn of
    DataType -> string "data "
    NewType  -> string "newtype "
  pretty tcdLName
  forM_ (hsq_explicit tcdTyVars) $ \x -> do
    space
    pretty x
  pretty tcdDataDefn
prettyTyClDecl ClassDecl {..} = do
  if isJust tcdCtxt
    then verHead
    else horHead <-|> verHead
  indentedBlock $ newlinePrefixed $ fmap pretty sigsMethodsFamilies
  where
    horHead = do
      string "class "
      printNameAndTypeVariables
      unless (null tcdFDs) $ do
        string " | "
        forM_ tcdFDs $ \(L _ (FunDep _ from to)) ->
          spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to
      unless (null sigsMethodsFamilies) $ string " where"
    verHead = do
      string "class " |=> do
        whenJust tcdCtxt $ \ctx -> do
          printCommentsAnd ctx $ \case
            []  -> string "()"
            [x] -> pretty x
            xs  -> hTuple $ fmap pretty xs
          string " =>"
          newline
        printNameAndTypeVariables
      unless (null tcdFDs) $ do
        newline
        indentedBlock $
          string "| " |=>
          vCommaSep
            (flip fmap tcdFDs $ \(L _ (FunDep _ from to)) ->
               spaced $ fmap pretty from ++ [string "->"] ++ fmap pretty to)
      unless (null sigsMethodsFamilies) $ do
        newline
        indentedBlock $ string "where"
    printNameAndTypeVariables =
      case tcdFixity of
        Prefix ->
          spaced $ pretty tcdLName : fmap pretty (hsq_explicit tcdTyVars)
        Infix ->
          case hsq_explicit tcdTyVars of
            (l:r:xs) -> do
              parens $
                spaced [pretty l, pretty $ fmap InfixOp tcdLName, pretty r]
              spacePrefixed $ fmap pretty xs
            _ -> error "Not enough parameters are given."
    sigsMethodsFamilies =
      mkSortedLSigBindFamilyList tcdSigs (bagToList tcdMeths) tcdATs

instance Pretty (InstDecl GhcPs) where
  pretty' ClsInstD {..}     = pretty cid_inst
  pretty' DataFamInstD {..} = pretty dfid_inst
  pretty' TyFamInstD {..}   = pretty tfid_inst

instance Pretty (HsBind GhcPs) where
  pretty' = prettyHsBind
  commentsBefore FunBind {..} = commentsBefore fun_id
  commentsBefore _            = []
  commentOnSameLine FunBind {..} = commentOnSameLine fun_id
  commentOnSameLine _            = Nothing
  commentsAfter FunBind {..} = commentsAfter fun_id
  commentsAfter _            = []

prettyHsBind :: HsBind GhcPs -> Printer ()
prettyHsBind FunBind {..} = pretty fun_matches
prettyHsBind PatBind {..} = do
  pretty pat_lhs
  pretty pat_rhs
prettyHsBind VarBind {} = undefined
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsBind AbsBinds {} = undefined
#endif
prettyHsBind (PatSynBind _ x) = pretty x

instance Pretty (Sig GhcPs) where
  pretty' (TypeSig _ funName params) = do
    printFunName
    horizontal <-|> vertical
    where
      horizontal = do
        string " :: "
        pretty $ hswc_body params
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then string " :: "
          else do
            string " ::"
            newline
        indentedBlock $
          indentedWithSpace 3 $
          pretty $ HsSigTypeInsideVerticalFuncSig <$> hswc_body params
      printFunName = pretty $ head funName
  pretty' (PatSynSig _ names sig) =
    spaced
      [string "pattern", hCommaSep $ fmap pretty names, string "::", pretty sig]
  pretty' (ClassOpSig _ True funNames params) = do
    string "default "
    hCommaSep $ fmap pretty funNames
    string " :: "
    printCommentsAnd params pretty
  pretty' (ClassOpSig _ False funNames params) = do
    hCommaSep $ fmap pretty funNames
    string " :: "
    printCommentsAnd params (pretty . HsSigTypeInsideDeclSig)
  pretty' IdSig {} = undefined
  pretty' (FixSig _ x) = pretty x
  pretty' (InlineSig _ name detail) = do
    string "{-# "
    pretty detail
    space
    pretty name
    string " #-}"
  pretty' (SpecSig _ name sig _) =
    spaced
      [ string "{-# SPECIALISE"
      , pretty name
      , string "::"
      , pretty $ head sig
      , string "#-}"
      ]
  pretty' (SpecInstSig _ _ sig) =
    spaced [string "{-# SPECIALISE instance", pretty sig, string "#-}"]
  pretty' (MinimalSig _ _ xs) =
    string "{-# MINIMAL " |=> do
      pretty xs
      string " #-}"
  pretty' (SCCFunSig _ _ name _) =
    spaced [string "{-# SCC", pretty name, string "#-}"]
  pretty' (CompleteMatchSig _ _ names _) =
    spaced
      [ string "{-# COMPLETE"
      , printCommentsAnd names (hCommaSep . fmap pretty)
      , string "#-}"
      ]
  commentsBefore (TypeSig x _ _) = commentsBefore x
  commentsBefore _               = []
  commentOnSameLine (TypeSig x _ _) = commentOnSameLine x
  commentOnSameLine _               = Nothing
  commentsAfter (TypeSig x _ _) = commentsAfter x
  commentsAfter _               = []

instance Pretty DeclSig where
  pretty' (DeclSig (TypeSig _ funName params)) = do
    printFunName
    horizontal <-|> vertical
    where
      horizontal = do
        string " :: "
        pretty $ HsSigTypeInsideDeclSig <$> hswc_body params
      vertical = do
        headLen <- printerLength printFunName
        indentSpaces <- getIndentSpaces
        if headLen < indentSpaces
          then string " :: "
          else do
            string " ::"
            newline
        indentedBlock $
          indentedWithSpace 3 $
          pretty $ HsSigTypeInsideDeclSig <$> hswc_body params
      printFunName = hCommaSep $ fmap pretty funName
  pretty' (DeclSig x) = pretty x
  commentsBefore (DeclSig x) = commentsBefore x
  commentOnSameLine (DeclSig x) = commentOnSameLine x
  commentsAfter (DeclSig x) = commentsAfter x

instance Pretty (HsDataDefn GhcPs) where
  pretty' HsDataDefn {..} =
    case dd_kindSig of
      Just kindSig -> do
        string " :: "
        pretty kindSig
        string " where"
        indentedBlock $ newlinePrefixed $ fmap pretty dd_cons
      Nothing ->
        indentedBlock $ do
          case dd_cons of
            [] -> pure ()
            [x] -> do
              string " ="
              newline
              pretty x
            _ -> do
              newline
              string "= " |=> vBarSep (fmap pretty dd_cons)
          unless (null dd_derivs) $ do
            newline
            lined $ fmap pretty dd_derivs

instance Pretty HsDataDefnForDataInstance where
  pretty' (HsDataDefnForDataInstance HsDataDefn {..}) =
    vBarSep $ fmap pretty dd_cons

instance Pretty (ClsInstDecl GhcPs) where
  pretty' ClsInstDecl {..} = do
    string "instance " |=> do
      whenJust cid_overlap_mode $ \x -> do
        pretty x
        space
      pretty (fmap HsSigTypeInsideInstDecl cid_poly_ty) |=>
        unless (null sigsAndMethods) (string " where")
    unless (null sigsAndMethods) $ do
      newline
      indentedBlock $ lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods =
        mkSortedLSigBindFamilyList cid_sigs (bagToList cid_binds) []

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)

instance Pretty MatchGroupForCase where
  pretty' (MatchGroupForCase MG {..}) =
    printCommentsAnd mg_alts (lined . fmap (pretty . fmap MatchForCase))

instance Pretty MatchGroupForLambda where
  pretty' (MatchGroupForLambda MG {..}) =
    printCommentsAnd mg_alts (lined . fmap (pretty . fmap MatchForLambda))

instance Pretty (HsExpr GhcPs) where
  pretty' = prettyHsExpr
  commentsBefore (HsVar _ x)   = commentsBefore x
  commentsBefore (HsApp x _ _) = commentsBefore x
  commentsBefore _             = []
  commentOnSameLine (HsVar _ x)   = commentOnSameLine x
  commentOnSameLine (HsApp x _ _) = commentOnSameLine x
  commentOnSameLine _             = Nothing
  commentsAfter (HsVar _ x)   = commentsAfter x
  commentsAfter (HsApp x _ _) = commentsAfter x
  commentsAfter _             = []

-- HIndent cannot handle CPP instructions inside a class instance
-- declaration well. That is why this function is defined.
prettyHsExpr :: HsExpr GhcPs -> Printer ()
prettyHsExpr (HsVar _ bind) = pretty $ fmap PrefixOp bind
prettyHsExpr (HsUnboundVar _ x) = pretty x
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr HsConLikeOut {} = undefined
prettyHsExpr HsRecFld {} = undefined
#endif
prettyHsExpr (HsOverLabel _ l) = do
  string "#"
  pretty l
prettyHsExpr HsIPVar {} = undefined
prettyHsExpr (HsOverLit _ x) = pretty x
prettyHsExpr (HsLit _ l) = pretty l
prettyHsExpr (HsLam _ body) = pretty $ MatchGroupForLambda body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLamCase _ _ matches) = do
  string "\\case"
  if null $ unLoc $ mg_alts matches
    then string " {}"
    else do
      newline
      indentedBlock $ pretty $ MatchGroupForCase matches
#else
prettyHsExpr (HsLamCase _ matches) = do
  string "\\case"
  if null $ unLoc $ mg_alts matches
    then string " {}"
    else do
      newline
      indentedBlock $ pretty $ MatchGroupForCase matches
#endif
prettyHsExpr (HsApp _ l r) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty l, pretty r]
    vertical = do
      let (f, args) =
            case flatten l ++ [r] of
              []         -> error "Invalid function application."
              (f':args') -> (f', args')
      col <- gets psColumn
      spaces <- getIndentSpaces
      pretty f
      col' <- gets psColumn
      let diff =
            col' - col -
            if col == 0
              then spaces
              else 0
      if diff + 1 <= spaces
        then space
        else newline
      spaces' <- getIndentSpaces
      indentedWithSpace spaces' $ lined $ fmap pretty args
    flatten :: LHsExpr GhcPs -> [LHsExpr GhcPs]
    flatten (L (SrcSpanAnn (EpAnn _ _ cs) _) (HsApp _ l' r')) =
      flatten l' ++ [insertComments cs r']
    flatten x = [x]
    insertComments :: EpAnnComments -> LHsExpr GhcPs -> LHsExpr GhcPs
    insertComments cs (L s@SrcSpanAnn {ann = e@EpAnn {comments = cs'}} r') =
      L (s {ann = e {comments = cs <> cs'}}) r'
    insertComments _ x = x
prettyHsExpr (HsAppType _ l r) = do
  pretty l
  space
  string "@"
  pretty r
prettyHsExpr (OpApp _ l o r) = pretty (InfixApp l o r False)
prettyHsExpr (NegApp _ x _) = do
  string "-"
  pretty x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsPar _ _ expr _) = parens $ pretty expr
#else
prettyHsExpr (HsPar _ expr) = parens $ pretty expr
#endif
prettyHsExpr (SectionL _ l o) = spaced [pretty l, pretty (InfixExpr o)]
prettyHsExpr (SectionR _ o r) = (pretty (InfixExpr o) >> space) |=> pretty r
prettyHsExpr (ExplicitTuple _ full _) = horizontal <-|> vertical
  where
    horizontal = hTuple $ fmap pretty full
    vertical =
      parens $
      prefixedLined "," $
      fmap (\e -> unless (isMissing e) (space |=> pretty e)) full
    isMissing Missing {} = True
    isMissing _          = False
prettyHsExpr ExplicitSum {} = undefined
prettyHsExpr (HsCase _ cond arms) = do
  string "case " |=> do
    pretty cond
    string " of"
  if null $ unLoc $ mg_alts arms
    then string " {}"
    else do
      newline
      indentedBlock $ pretty $ MatchGroupForCase arms
prettyHsExpr (HsIf _ cond t f) = do
  string "if " |=> pretty cond
  indentedBlock $ newlinePrefixed [branch "then " t, branch "else " f]
  where
    branch :: String -> LHsExpr GhcPs -> Printer ()
    branch str e =
      case e of
        (L _ (HsDo _ DoExpr {} xs)) -> do
          string str
          string "do"
          newline
          indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
        _ -> string str |=> pretty e
prettyHsExpr (HsMultiIf _ guards) =
  string "if " |=> lined (fmap (pretty . fmap GRHSForMultiwayIf) guards)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLet _ _ binds _ exprs) = do
  string "let " |=> pretty binds
  newline
  string " in " |=> pretty exprs
#else
prettyHsExpr (HsLet _ binds exprs) = do
  string "let " |=> pretty binds
  newline
  string " in " |=> pretty exprs
#endif
prettyHsExpr (HsDo _ (DoExpr _) xs) =
  string "do " |=> printCommentsAnd xs (lined . fmap pretty)
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
prettyHsExpr (HsDo _ MonadComp xs) = horizontal <-|> vertical
  where
    horizontal =
      brackets $ do
        printCommentsAnd xs (pretty . head)
        string " | "
        printCommentsAnd xs (hCommaSep . fmap pretty . tail)
    vertical =
      if null $ unLoc xs
        then string "[]"
        else printCommentsAnd
               xs
               (\xs' ->
                  let (lastStmt, others) = (head xs', tail xs')
                   in do string "[ "
                         pretty $ fmap StmtLRInsideVerticalList lastStmt
                         newline
                         forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
                           string p |=> pretty (fmap StmtLRInsideVerticalList x)
                           newline
                         string "]")
    stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)
prettyHsExpr HsDo {} = undefined
prettyHsExpr (ExplicitList _ xs) = horizontal <-|> vertical
  where
    horizontal = brackets $ hCommaSep $ fmap pretty xs
    vertical = vList $ fmap pretty xs
prettyHsExpr (RecordCon _ name fields) = horizontal <-|> vertical
  where
    horizontal = spaced [pretty name, pretty fields]
    vertical = do
      pretty name
      (space >> pretty fields) <-|> (newline >> indentedBlock (pretty fields))
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (RecordUpd _ name fields) = hor <-|> ver
  where
    hor = do
      pretty name
      space
      either printHorFields printHorFields fields
    ver = do
      pretty name
      newline
      indentedBlock $ either printVerFields printVerFields fields
    printHorFields ::
         (Pretty a, Pretty b, Pretty l)
      => [GenLocated l (HsFieldBind a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, Pretty l)
      => [GenLocated l (HsFieldBind a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField HsFieldBind {..} = do
      pretty hfbLHS
      string " = "
      pretty hfbRHS
    verField HsFieldBind {..} = do
      pretty hfbLHS
      string " ="
      newline
      indentedBlock $ pretty hfbRHS
#else
prettyHsExpr (RecordUpd _ name fields) = hor <-|> ver
  where
    hor = do
      pretty name
      space
      either printHorFields printHorFields fields
    ver = do
      pretty name
      newline
      indentedBlock $ either printVerFields printVerFields fields
    printHorFields ::
         (Pretty a, Pretty b, Pretty l)
      => [GenLocated l (HsRecField' a b)]
      -> Printer ()
    printHorFields = hFields . fmap (`printCommentsAnd` horField)
    printVerFields ::
         (Pretty a, Pretty b, Pretty l)
      => [GenLocated l (HsRecField' a b)]
      -> Printer ()
    printVerFields = vFields . fmap printField
    printField x = printCommentsAnd x $ (<-|>) <$> horField <*> verField
    horField HsRecField {..} = do
      pretty hsRecFieldLbl
      string " = "
      pretty hsRecFieldArg
    verField HsRecField {..} = do
      pretty hsRecFieldLbl
      string " ="
      newline
      indentedBlock $ pretty hsRecFieldArg
#endif
prettyHsExpr (HsGetField _ e f) = do
  pretty e
  dot
  pretty f
prettyHsExpr HsProjection {} = undefined
prettyHsExpr (ExprWithTySig _ e sig) = do
  pretty e
  string " :: "
  pretty $ hswc_body sig
prettyHsExpr (ArithSeq _ _ x) = pretty x
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsBracket _ inner) = pretty inner
prettyHsExpr HsRnBracketOut {} = undefined
prettyHsExpr HsTcBracketOut {} = undefined
#endif
prettyHsExpr (HsSpliceE _ x) = pretty x
prettyHsExpr HsProc {} = undefined
prettyHsExpr HsStatic {} = undefined
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr HsTick {} = undefined
prettyHsExpr HsBinTick {} = undefined
#endif
prettyHsExpr (HsPragE _ p x) = spaced [pretty p, pretty x]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr HsRecSel {} = undefined
prettyHsExpr HsTypedBracket {} = undefined
prettyHsExpr (HsUntypedBracket _ inner) = pretty inner
#endif
instance Pretty (HsSigType GhcPs) where
  pretty' HsSig {..} = do
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        space
      _ -> return ()
    pretty sig_body

instance Pretty HsSigTypeInsideInstDecl where
  pretty' (HsSigTypeInsideInstDecl HsSig {..}) = do
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        space
      _ -> return ()
    pretty $ fmap HsTypeInsideInstDecl sig_body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty HsSigTypeInsideVerticalFuncSig where
  pretty' (HsSigTypeInsideVerticalFuncSig HsSig {..}) =
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        printCommentsAnd sig_body $ \case
          HsQualTy {..} -> do
            (space >> pretty (HorizontalContext hst_ctxt)) <-|>
              (newline >> pretty (VerticalContext hst_ctxt))
            newline
            prefixed "=> " $ pretty hst_body
          x -> pretty $ HsTypeInsideDeclSig x
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body

instance Pretty HsSigTypeInsideDeclSig where
  pretty' (HsSigTypeInsideDeclSig HsSig {..}) =
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        case unLoc sig_body of
          HsQualTy {..} ->
            printCommentsAnd sig_body $ \_ ->
              let hor = do
                    space
                    pretty $ HorizontalContext hst_ctxt
                  ver = do
                    newline
                    pretty $ VerticalContext hst_ctxt
               in do hor <-|> ver
                     newline
                     prefixed "=> " $
                       prefixedLined "-> " $ pretty <$> flatten hst_body
          _ -> do
            space
            pretty $ fmap HsTypeInsideDeclSig sig_body
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
    where
      flatten :: LHsType GhcPs -> [LHsType GhcPs]
      flatten (L _ (HsFunTy _ _ l r)) = flatten l ++ flatten r
      flatten x                       = [x]
#else
instance Pretty HsSigTypeInsideVerticalFuncSig where
  pretty' (HsSigTypeInsideVerticalFuncSig HsSig {..}) =
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        printCommentsAnd sig_body $ \case
          HsQualTy {..} -> do
            (space >> pretty (HorizontalContext hst_ctxt)) <-|>
              (newline >> pretty (VerticalContext hst_ctxt))
            newline
            prefixed "=> " $ pretty hst_body
          x -> pretty $ HsTypeInsideDeclSig x
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body

instance Pretty HsSigTypeInsideDeclSig where
  pretty' (HsSigTypeInsideDeclSig HsSig {..}) =
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap pretty xs
        dot
        case unLoc sig_body of
          HsQualTy {..} ->
            printCommentsAnd sig_body $ \_ ->
              let hor = do
                    space
                    pretty $ HorizontalContext hst_ctxt
                  ver = do
                    newline
                    pretty $ VerticalContext hst_ctxt
               in do hor <-|> ver
                     newline
                     prefixed "=> " $
                       prefixedLined "-> " $ pretty <$> flatten hst_body
          _ -> do
            space
            pretty $ fmap HsTypeInsideDeclSig sig_body
      _ -> pretty $ fmap HsTypeInsideDeclSig sig_body
    where
      flatten :: LHsType GhcPs -> [LHsType GhcPs]
      flatten (L _ (HsFunTy _ _ l r)) = flatten l ++ flatten r
      flatten x                       = [x]
#endif
instance Pretty (ConDecl GhcPs) where
  pretty' = prettyConDecl

prettyConDecl :: ConDecl GhcPs -> Printer ()
prettyConDecl ConDeclGADT {..} = horizontal <-|> vertical
  where
    horizontal = do
      pretty $ head con_names
      string " :: "
      pretty con_g_args
      string " -> "
      pretty con_res_ty
    vertical = do
      pretty $ head con_names
      newline
      indentedBlock $ do
        string ":: " |=> pretty con_g_args
        newline
        string "-> "
        pretty con_res_ty
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyConDecl ConDeclH98 {con_forall = True, ..} =
  (do string "forall "
      spaced $ fmap pretty con_ex_tvs
      string ". ") |=>
  (do whenJust con_mb_cxt $ \c -> do
        pretty $ Context c
        string " =>"
        newline
      pretty con_name
      pretty con_args)
#else
prettyConDecl ConDeclH98 {con_forall = True, ..} =
  (do string "forall "
      spaced $ fmap pretty con_ex_tvs
      string ". ") |=>
  (do whenJust con_mb_cxt $ \_ -> do
        pretty $ Context con_mb_cxt
        string " =>"
        newline
      pretty con_name
      pretty con_args)
#endif
prettyConDecl ConDeclH98 {con_forall = False, ..} =
  case con_args of
    (InfixCon l r) ->
      spaced [pretty l, pretty $ fmap InfixOp con_name, pretty r]
    _ -> do
      pretty con_name
      pretty con_args

instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' Match {..} =
    case mc_fixity m_ctxt of
      Prefix -> do
        pretty m_ctxt
        spacePrefixed $ fmap pretty m_pats
        pretty m_grhss
      Infix -> do
        case (m_pats, m_ctxt) of
          (l:r:xs, FunRhs {..}) -> do
            spaced $
              [pretty l, pretty $ fmap InfixOp mc_fun, pretty r] ++
              fmap pretty xs
            pretty m_grhss
          _ -> error "Not enough parameters are passed."
  commentsBefore Match {..} = commentsBefore m_ext
  commentOnSameLine Match {..} = commentOnSameLine m_ext
  commentsAfter Match {..} = commentsAfter m_ext

instance Pretty MatchForCase where
  pretty' (MatchForCase Match {..}) = do
    mapM_ pretty m_pats
    pretty (GRHSsForCase m_grhss)
  commentsBefore (MatchForCase x) = commentsBefore x
  commentOnSameLine (MatchForCase x) = commentOnSameLine x
  commentsAfter (MatchForCase x) = commentsAfter x

instance Pretty MatchForLambda where
  pretty' (MatchForLambda Match {..}) = do
    string "\\"
    unless (null m_pats) $
      case unLoc $ head m_pats of
        LazyPat {} -> space
        BangPat {} -> space
        _          -> return ()
    spaced $ fmap pretty m_pats ++ [pretty $ GRHSsForLambda m_grhss]
  commentsBefore (MatchForLambda x) = commentsBefore x
  commentOnSameLine (MatchForLambda x) = commentOnSameLine x
  commentsAfter (MatchForLambda x) = commentsAfter x

instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' (LastStmt _ x _ _) = pretty x
  pretty' (BindStmt _ pat body) = hor <-|> ver
    where
      hor = spaced [pretty pat, string "<-", pretty body]
      ver = do
        pretty pat
        string " <-"
        newline
        indentedBlock $ pretty body
  pretty' ApplicativeStmt {} = undefined
  pretty' (BodyStmt _ (L loc (OpApp _ l o r)) _ _) =
    pretty (L loc (InfixApp l o r True))
  pretty' (BodyStmt _ body _ _) = pretty body
  pretty' (LetStmt _ l) = string "let " |=> pretty l
  pretty' (ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {} = undefined
  commentsBefore (LetStmt l _) = commentsBefore l
  commentsBefore _             = []
  commentsAfter (LetStmt l _) = commentsAfter l
  commentsAfter _             = []

instance Pretty StmtLRInsideVerticalList where
  pretty' (StmtLRInsideVerticalList (ParStmt _ xs _ _)) =
    vBarSep $ fmap (pretty . ParStmtBlockInsideVerticalList) xs
  pretty' (StmtLRInsideVerticalList x) = pretty x

-- | For pattern matching.
instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _  -> braces $ string ".."
          Nothing -> hFields $ fmap pretty rec_flds
      vertical = vFields $ fmap pretty rec_flds

-- | For record updates
instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsRecFields {..} = hvFields fieldPrinters
    where
      fieldPrinters =
        fmap pretty rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)

instance Pretty (HsType GhcPs) where
  pretty' = prettyHsType

prettyHsType :: HsType GhcPs -> Printer ()
prettyHsType (HsForAllTy _ tele body) = (pretty tele >> space) |=> pretty body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsType HsQualTy {..} = notVer
  where
    notVer = do
      pretty (Context hst_ctxt)
      string " =>"
      newline
      indentedBlock $ pretty hst_body
#else
prettyHsType HsQualTy {..} = notVer
  where
    notVer = do
      pretty (Context hst_ctxt)
      string " =>"
      newline
      indentedBlock $ pretty hst_body
#endif
prettyHsType (HsTyVar _ NotPromoted x) = pretty x
prettyHsType (HsTyVar _ IsPromoted x) = do
  string "'"
  pretty x
prettyHsType (HsAppTy _ l r) = spaced $ fmap pretty [l, r]
prettyHsType HsAppKindTy {} = undefined
prettyHsType (HsFunTy _ _ a b) = hor <-|> noDeclSigV
  where
    hor = spaced [pretty a, string "->", pretty b]
    noDeclSigV = do
      pretty a
      newline
      prefixed "-> " $ pretty b
prettyHsType (HsListTy _ xs) = brackets $ pretty xs
prettyHsType (HsTupleTy _ _ xs) = hvTuple' $ fmap pretty xs
prettyHsType HsSumTy {} = undefined
  -- For `HsOpTy`, we do not need a single quote for the infix operator. An
  -- explicit promotion is necessary if there is a data constructor and
  -- a type with the same name. However, infix data constructors never
  -- share their names with types because types cannot contain symbols.
  -- Thus there is no ambiguity.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsType (HsOpTy _ _ l op r) =
  spaced [pretty l, pretty $ fmap InfixOp op, pretty r]
#else
prettyHsType (HsOpTy _ l op r) =
  spaced [pretty l, pretty $ fmap InfixOp op, pretty r]
#endif
prettyHsType (HsParTy _ inside) = parens $ pretty inside
prettyHsType (HsIParamTy _ x ty) = do
  string "?"
  spaced [pretty x, string "::", pretty ty]
prettyHsType HsStarTy {} = string "*"
prettyHsType (HsKindSig _ t k) = spaced [pretty t, string "::", pretty k]
prettyHsType (HsSpliceTy _ sp) = pretty sp
prettyHsType HsDocTy {} = undefined
prettyHsType (HsBangTy _ _ x) = do
  string "!"
  pretty x
prettyHsType HsRecTy {} = undefined
prettyHsType (HsExplicitListTy _ _ xs) =
  case xs of
    [] -> string "'[]"
    _  -> hPromotedList $ fmap pretty xs
prettyHsType (HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap pretty xs
prettyHsType (HsTyLit _ x) = pretty x
prettyHsType HsWildCardTy {} = undefined
prettyHsType XHsType {} = undefined
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty HsTypeInsideInstDecl where
  pretty' (HsTypeInsideInstDecl HsQualTy {..}) = hor <-|> notVer
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      notVer = do
        pretty (Context hst_ctxt)
        string " =>"
        newline
        pretty hst_body
  pretty' (HsTypeInsideInstDecl x) = pretty x

instance Pretty HsTypeInsideDeclSig where
  pretty' (HsTypeInsideDeclSig HsQualTy {..}) = hor <-|> sigVer
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      sigVer = do
        pretty (Context hst_ctxt)
        newline
        prefixed "=> " $ pretty $ fmap HsTypeInsideVerticalDeclSig hst_body
  pretty' (HsTypeInsideDeclSig (HsFunTy _ _ a b)) = hor <-|> declSigV
    where
      hor = spaced [pretty a, string "->", pretty b]
      declSigV = do
        pretty $ fmap HsTypeInsideVerticalDeclSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalDeclSig b
  pretty' (HsTypeInsideDeclSig x) = pretty x
#else
instance Pretty HsTypeInsideInstDecl where
  pretty' (HsTypeInsideInstDecl HsQualTy {..}) = hor <-|> notVer
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      notVer = do
        pretty (Context hst_ctxt)
        string " =>"
        newline
        pretty hst_body
  pretty' (HsTypeInsideInstDecl x) = pretty x

instance Pretty HsTypeInsideDeclSig where
  pretty' (HsTypeInsideDeclSig HsQualTy {..}) = hor <-|> sigVer
    where
      hor = spaced [pretty (Context hst_ctxt), string "=>", pretty hst_body]
      sigVer = do
        pretty (Context hst_ctxt)
        newline
        prefixed "=> " $ pretty $ fmap HsTypeInsideVerticalDeclSig hst_body
  pretty' (HsTypeInsideDeclSig (HsFunTy _ _ a b)) = hor <-|> declSigV
    where
      hor = spaced [pretty a, string "->", pretty b]
      declSigV = do
        pretty $ fmap HsTypeInsideVerticalDeclSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalDeclSig b
  pretty' (HsTypeInsideDeclSig x) = pretty x
  commentsBefore (HsTypeInsideDeclSig x) = commentsBefore x
  commentOnSameLine (HsTypeInsideDeclSig x) = commentOnSameLine x
  commentsAfter (HsTypeInsideDeclSig x) = commentsAfter x
#endif
instance Pretty HsTypeInsideVerticalFuncSig where
  pretty' (HsTypeInsideVerticalFuncSig (HsFunTy _ _ a b)) = noDeclSigV
    where
      noDeclSigV = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideVerticalFuncSig x) = pretty x

instance Pretty HsTypeInsideVerticalDeclSig where
  pretty' (HsTypeInsideVerticalDeclSig (HsFunTy _ _ a b)) = declSigV
    where
      declSigV = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideVerticalDeclSig x) = pretty x
  commentsBefore (HsTypeInsideVerticalDeclSig x) = commentsBefore x
  commentOnSameLine (HsTypeInsideVerticalDeclSig x) = commentOnSameLine x
  commentsAfter (HsTypeInsideVerticalDeclSig x) = commentsAfter x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsConDeclGADTDetails GhcPs) where
  pretty' (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> pretty x
  pretty' (RecConGADT xs _) =
    printCommentsAnd xs $ \xs' ->
      vFields' $
      flip fmap xs' $ \(L _ ConDeclField {..}) -> do
        pretty $ head cd_fld_names
        string " :: "
        pretty cd_fld_type
#else
instance Pretty (HsConDeclGADTDetails GhcPs) where
  pretty' (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> pretty x
  pretty' (RecConGADT xs) =
    printCommentsAnd xs $ \xs' ->
      vFields' $
      flip fmap xs' $ \(L _ ConDeclField {..}) -> do
        pretty $ head cd_fld_names
        string " :: "
        pretty cd_fld_type
#endif
instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' GRHSs {..} = do
    mapM_ pretty grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedBlock $
        newlinePrefixed
          [string "where", printCommentsAnd (L epa lr) (indentedBlock . pretty)]
      _ -> return ()

instance Pretty GRHSsForCase where
  pretty' (GRHSsForCase GRHSs {..}) = do
    mapM_ (pretty . fmap GRHSForCase) grhssGRHSs
    case grhssLocalBinds of
      HsValBinds {} ->
        indentedBlock $ do
          newline
          string "where " |=> pretty grhssLocalBinds
      _ -> pure ()

instance Pretty GRHSsForLambda where
  pretty' (GRHSsForLambda GRHSs {..}) = do
    mapM_ (pretty . fmap GRHSForLambda) grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedBlock $
        newlinePrefixed
          [string "where", printCommentsAnd (L epa lr) (indentedBlock . pretty)]
      _ -> return ()

instance Pretty (HsMatchContext GhcPs) where
  pretty' = prettyHsMatchContext

prettyHsMatchContext :: HsMatchContext GhcPs -> Printer ()
prettyHsMatchContext FunRhs {..}       = pretty mc_fun
prettyHsMatchContext LambdaExpr        = return ()
prettyHsMatchContext CaseAlt           = return ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsMatchContext LamCaseAlt {}     = undefined
#endif
prettyHsMatchContext IfAlt {}          = undefined
prettyHsMatchContext ArrowMatchCtxt {} = undefined
prettyHsMatchContext PatBindRhs {}     = undefined
prettyHsMatchContext PatBindGuards {}  = undefined
prettyHsMatchContext RecUpd {}         = undefined
prettyHsMatchContext StmtCtxt {}       = undefined
prettyHsMatchContext ThPatSplice {}    = undefined
prettyHsMatchContext ThPatQuote {}     = undefined
prettyHsMatchContext PatSyn {}         = undefined

instance Pretty (ParStmtBlock GhcPs GhcPs) where
  pretty' (ParStmtBlock _ xs _ _) = hvCommaSep $ fmap pretty xs

instance Pretty ParStmtBlockInsideVerticalList where
  pretty' (ParStmtBlockInsideVerticalList (ParStmtBlock _ xs _ _)) =
    vCommaSep $ fmap pretty xs

instance Pretty RdrName where
  pretty' = pretty . PrefixOp

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' (GRHS _ [] (L _ (HsDo _ (DoExpr _) body))) = do
    string " = do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHS _ guards (L _ (HsDo _ (DoExpr _) body))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " = do"
      hor <-|> ver
    where
      hor = do
        space
        printCommentsAnd body (lined . fmap pretty)
      ver = do
        newline
        indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHS _ [] body) = do
    string " ="
    horizontal <-|> vertical
    where
      horizontal = do
        space
        pretty body
      vertical = do
        newline
        indentedBlock $ pretty body
  pretty' (GRHS _ guards body) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      horizontal <-|> vertical
    where
      horizontal = do
        string " = "
        pretty body
      vertical = do
        string " ="
        newline
        indentedBlock $ pretty body
  commentsBefore (GRHS x _ _) = commentsBefore x
  commentOnSameLine (GRHS x _ _) = commentOnSameLine x
  commentsAfter (GRHS x _ _) = commentsAfter x

instance Pretty GRHSForCase where
  pretty' (GRHSForCase (GRHS _ [] (L _ (HsDo _ (DoExpr _) body)))) = do
    string " -> do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForCase (GRHS _ guards (L _ (HsDo _ (DoExpr _) body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForCase (GRHS _ [] body)) = horizontal <-|> vertical
    where
      horizontal = do
        string " -> "
        pretty body
      vertical = do
        string " ->"
        newline
        indentedBlock $ pretty body
  pretty' (GRHSForCase (GRHS _ guards body)) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      horizontal <-|> vertical
    where
      horizontal = do
        string " -> "
        pretty body
      vertical = do
        string " ->"
        newline
        indentedBlock $ pretty body
  commentsBefore (GRHSForCase (GRHS x _ _)) = commentsBefore x
  commentOnSameLine (GRHSForCase (GRHS x _ _)) = commentOnSameLine x
  commentsAfter (GRHSForCase (GRHS x _ _)) = commentsAfter x

instance Pretty GRHSForLambda where
  pretty' (GRHSForLambda (GRHS _ [] (L _ (HsDo _ (DoExpr _) body)))) =
    hor <-|> ver
    where
      hor = do
        string "-> do "
        printCommentsAnd body (lined . fmap pretty)
      ver = do
        string "-> do"
        newline
        indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForLambda (GRHS _ guards (L _ (HsDo _ (DoExpr _) body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForLambda (GRHS _ [] body)) = horizontal <-|> vertical
    where
      horizontal = do
        string "-> "
        pretty body
      vertical = do
        string "->"
        newline
        indentedBlock $ pretty body
  pretty' (GRHSForLambda (GRHS _ guards body)) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      horizontal <-|> vertical
    where
      horizontal = do
        string " -> "
        pretty body
      vertical = do
        string " ->"
        newline
        indentedBlock $ pretty body
  commentsBefore (GRHSForLambda (GRHS x _ _)) = commentsBefore x
  commentOnSameLine (GRHSForLambda (GRHS x _ _)) = commentOnSameLine x
  commentsAfter (GRHSForLambda (GRHS x _ _)) = commentsAfter x

instance Pretty GRHSForMultiwayIf where
  pretty' (GRHSForMultiwayIf (GRHS _ [] (L _ (HsDo _ (DoExpr _) body)))) = do
    string " -> do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForMultiwayIf (GRHS _ guards (L _ (HsDo _ (DoExpr _) body)))) =
    indentedBlock $ do
      string "| "
      inter (comma >> newline) $ fmap pretty guards
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForMultiwayIf (GRHS _ [] body)) = horizontal <-|> vertical
    where
      horizontal = do
        string " -> "
        pretty body
      vertical = do
        string " ->"
        newline
        indentedBlock $ pretty body
  pretty' (GRHSForMultiwayIf (GRHS _ guards body)) =
    string "| " |=> do
      inter (comma >> newline) $ fmap pretty guards
      horizontal <-|> vertical
    where
      horizontal = spacePrefixed [string "->", pretty body]
      vertical = do
        string " ->"
        newline
        pretty body
  commentsBefore (GRHSForMultiwayIf (GRHS x _ _)) = commentsBefore x
  commentOnSameLine (GRHSForMultiwayIf (GRHS x _ _)) = commentOnSameLine x
  commentsAfter (GRHSForMultiwayIf (GRHS x _ _)) = commentsAfter x

instance Pretty EpaCommentTok where
  pretty' (EpaLineComment c) = string c
  pretty' (EpaBlockComment c) =
    case lines c of
      [] -> pure ()
      [x] -> string x
      (x:xs) -> do
        string x
        newline
        -- 'indentedWithFixedLevel 0' is used because an 'EpaBlockComment'
        -- contains indent spaces for all lines except the first one.
        indentedWithFixedLevel 0 $ lined $ fmap string xs
  pretty' _ = undefined

instance Pretty (SpliceDecl GhcPs) where
  pretty' (SpliceDecl _ sp _) = pretty sp

instance Pretty (HsSplice GhcPs) where
  pretty' HsTypedSplice {} = undefined
  pretty' (HsUntypedSplice _ decoration _ body) = do
    string prefix
    pretty body
    where
      prefix =
        case decoration of
          DollarSplice -> "$"
          BareSplice   -> ""
  pretty' (HsQuasiQuote _ _ l _ r) =
    brackets $ do
      pretty l
      string "|"
      pretty r
      string "|"
  pretty' HsSpliced {} = undefined

instance Pretty (Pat GhcPs) where
  pretty' = prettyPat

prettyPat :: Pat GhcPs -> Printer ()
prettyPat WildPat {} = string "_"
prettyPat (VarPat _ x) = pretty x
prettyPat (LazyPat _ x) = do
  string "~"
  pretty x
prettyPat (AsPat _ a b) = do
  pretty a
  string "@"
  pretty b
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyPat (ParPat _ _ inner _) = parens $ pretty inner
#else
prettyPat (ParPat _ inner) = parens $ pretty inner
#endif
prettyPat (BangPat _ x) = do
  string "!"
  pretty x
prettyPat (ListPat _ xs) = hList $ fmap pretty xs
prettyPat (TuplePat _ pats _) = hTuple $ fmap pretty pats
prettyPat SumPat {} = undefined
prettyPat ConPat {..} =
  case pat_args of
    PrefixCon _ as -> do
      pretty $ fmap PrefixOp pat_con
      spacePrefixed $ fmap pretty as
    RecCon rec -> (pretty pat_con >> space) |=> pretty (RecConPat rec)
    InfixCon a b -> do
      pretty a
      unlessSpecialOp (unLoc pat_con) space
      pretty $ fmap InfixOp pat_con
      unlessSpecialOp (unLoc pat_con) space
      pretty b
prettyPat (ViewPat _ l r) = spaced [pretty l, string "->", pretty r]
prettyPat (SplicePat _ x) = pretty x
prettyPat (LitPat _ x) = pretty x
prettyPat (NPat _ x _ _) = pretty x
prettyPat (NPlusKPat _ n k _ _ _) = do
  pretty n
  string "+"
  pretty k
prettyPat (SigPat _ l r) = spaced [pretty l, string "::", pretty r]

instance Pretty RecConPat where
  pretty' (RecConPat HsRecFields {..}) =
    case fieldPrinters of
      []  -> string "{}"
      [x] -> braces x
      xs  -> hvFields xs
    where
      fieldPrinters =
        fmap (pretty . fmap RecConField) rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsBracket GhcPs) where
  pretty' (ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty' (PatBr _ expr) =
    brackets $ do
      string "p"
      wrapWithBars $ pretty expr
  pretty' DecBrL {} = undefined
  pretty' DecBrG {} = undefined
  pretty' (TypBr _ expr) =
    brackets $ do
      string "t"
      wrapWithBars $ pretty expr
  pretty' (VarBr _ True var) = do
    string "'"
    pretty var
  pretty' (VarBr _ False var) = do
    string "''"
    pretty var
  pretty' TExpBr {} = undefined
#endif
instance Pretty SigBindFamily where
  pretty' (Sig x)        = pretty $ DeclSig x
  pretty' (Bind x)       = pretty x
  pretty' (TypeFamily x) = pretty x

instance Pretty EpaComment where
  pretty' EpaComment {..} = pretty ac_tok

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'Anchor'.
instance Pretty Anchor where
  pretty' _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'SrcAnn'.
instance Pretty (SrcAnn a) where
  pretty' _ = return ()
  commentsBefore (SrcSpanAnn ep _) = commentsBefore ep
  commentOnSameLine (SrcSpanAnn ep _) = commentOnSameLine ep
  commentsAfter (SrcSpanAnn ep _) = commentsAfter ep

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'SrcSpan'.
instance Pretty SrcSpan where
  pretty' _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'EpAnn'.
instance Pretty (EpAnn a) where
  pretty' _ = return ()
  commentsBefore (EpAnn _ _ cs) = priorComments cs
  commentsBefore EpAnnNotUsed   = []
  -- FIXME: Remove duplicated 'where's.
  commentOnSameLine (EpAnn ann _ cs) = find isSameLine $ getFollowingComments cs
    where
      isSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  commentOnSameLine EpAnnNotUsed = Nothing
  commentsAfter (EpAnn ann _ cs) =
    filter (not . isSameLine) $ getFollowingComments cs
    where
      isSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  commentsAfter EpAnnNotUsed = []

instance Pretty (HsLocalBindsLR GhcPs GhcPs) where
  pretty' (HsValBinds _ lr)  = pretty lr
  pretty' (HsIPBinds _ x)    = pretty x
  pretty' EmptyLocalBinds {} = undefined

instance Pretty (HsValBindsLR GhcPs GhcPs) where
  pretty' (ValBinds _ methods sigs) = lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods = mkSortedLSigBindFamilyList sigs (bagToList methods) []
  pretty' XValBindsLR {} = undefined

instance Pretty (HsTupArg GhcPs) where
  pretty' (Present _ e) = pretty e
  pretty' Missing {}    = pure () -- This appears in a tuple section.
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty RecConField where
  pretty' (RecConField HsFieldBind {..}) = do
    pretty hfbLHS
    unless hfbPun $ do
      string " = "
      pretty hfbRHS
#else
-- | For pattern matching against a record.
instance Pretty (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsRecField {..} =
    (pretty hsRecFieldLbl >> string " = ") |=> pretty hsRecFieldArg

-- | For record updates.
instance Pretty (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsRecField {..} = horizontal <-|> vertical
    where
      horizontal = do
        pretty hsRecFieldLbl
        unless hsRecPun $ do
          string " = "
          pretty hsRecFieldArg
      vertical = do
        pretty hsRecFieldLbl
        unless hsRecPun $ do
          string " ="
          newline
          indentedBlock $ pretty hsRecFieldArg
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | For pattern matchings against records.
instance Pretty (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs)) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsFieldBind {..} = (pretty hfbLHS >> string " = ") |=> pretty hfbRHS

-- | For record updates.
instance Pretty (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs)) (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsFieldBind {..} = horizontal <-|> vertical
    where
      horizontal = do
        pretty hfbLHS
        unless hfbPun $ do
          string " = "
          pretty hfbRHS
      vertical = do
        pretty hfbLHS
        unless hfbPun $ do
          string " ="
          newline
          indentedBlock $ pretty hfbRHS
#else
instance Pretty RecConField where
  pretty' (RecConField HsRecField {..}) = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " = "
      pretty hsRecFieldArg
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty foLabel
#else
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty rdrNameFieldOcc
#endif
-- HsConDeclH98Details
instance Pretty (HsConDetails Void (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs))) (GenLocated SrcSpanAnnL [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  pretty' (PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap pretty xs
      vertical = indentedBlock $ newlinePrefixed $ fmap pretty xs
  pretty' (RecCon (L _ rec)) = do
    newline
    indentedBlock $ vFields $ fmap pretty rec
  pretty' InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."

-- FIXME: Reconsider using a type variable.
instance Pretty a => Pretty (HsScaled GhcPs a) where
  pretty' (HsScaled _ x) = pretty x

instance Pretty (ConDeclField GhcPs) where
  pretty' ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    hCommaSep $ fmap pretty cd_fld_names
    string " :: "
    pretty cd_fld_type

instance Pretty InfixExpr where
  pretty' (InfixExpr (L _ (HsVar _ bind))) = pretty $ fmap InfixOp bind
  pretty' (InfixExpr x)                    = pretty' x
  commentsBefore (InfixExpr x) = commentsBefore x
  commentOnSameLine (InfixExpr x) = commentOnSameLine x
  commentsAfter (InfixExpr x) = commentsAfter x

instance Pretty InfixApp where
  pretty' InfixApp {..} = horizontal <-|> vertical
    where
      horizontal = spaced [pretty lhs, pretty (InfixExpr op), pretty rhs]
      vertical = do
        lhsVer
        beforeRhs <-
          case unLoc lhs of
            (HsDo _ DoExpr {} _) -> do
              indentedWithSpace 3 (newline >> pretty (InfixExpr op)) -- 3 for "do "
              return space
            _ -> do
              space
              pretty (InfixExpr op)
              return newline
        case unLoc rhs of
          (HsDo _ (DoExpr _) xs) -> do
            string " do"
            newline
            indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
          HsLam {} -> do
            space
            pretty rhs
          HsLamCase {} -> do
            space
            pretty rhs
          _ ->
            (if immediatelyAfterDo
               then indentedBlock
               else id) $ do
              beforeRhs
              col <- startingColumn
              (if col == 0
                 then indentedBlock
                 else id) $
                pretty rhs
      lhsVer =
        case lhs of
          (L loc (OpApp _ l o r)) ->
            pretty (L loc (InfixApp l o r immediatelyAfterDo))
          _ -> pretty lhs

instance Pretty a => Pretty (BooleanFormula a) where
  pretty' (Var x)    = pretty x
  pretty' (And xs)   = hvCommaSep $ fmap pretty xs
  pretty' (Or xs)    = hvBarSep $ fmap pretty xs
  pretty' (Parens x) = parens $ pretty x

instance Pretty (FieldLabelStrings GhcPs) where
  pretty' _ = undefined

instance Pretty (AmbiguousFieldOcc GhcPs) where
  pretty' (Unambiguous _ name) = pretty name
  pretty' (Ambiguous _ name)   = pretty name

instance Pretty (ImportDecl GhcPs) where
  pretty' decl@ImportDecl {..} = do
    string "import "
    when (ideclSource == IsBoot) $ string "{-# SOURCE #-} "
    when ideclSafe $ string "safe "
    unless (ideclQualified == NotQualified) $ string "qualified "
    whenJust (packageName decl) $ \x -> do
      pretty x
      space
    pretty ideclName
    whenJust ideclAs $ \x -> do
      string " as "
      pretty x
    whenJust ideclHiding $ \(x, _) -> do
      when x (string " hiding")
      (string " " >> hTuple explicitOrHidingImports) <-|>
        (newline >> indentedBlock (vTuple explicitOrHidingImports))
      -- TODO: Handle comments.
    where
      explicitOrHidingImports =
        pretty <$> maybe [] (fmap unLoc . unLoc . snd) ideclHiding

packageName :: ImportDecl GhcPs -> Maybe StringLiteral
#if MIN_VERSION_ghc_lib_parser(9,4,1)
packageName (ideclPkgQual -> RawPkgQual name) = Just name
packageName _ = Nothing
#else
packageName = ideclPkgQual
#endif
instance Pretty (HsDerivingClause GhcPs) where
  pretty' HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      pretty x
      space
    pretty deriv_clause_tys

instance Pretty (DerivClauseTys GhcPs) where
  pretty' (DctSingle _ ty) = parens $ pretty ty
  pretty' (DctMulti _ ts)  = hvTuple $ fmap pretty ts

instance Pretty OverlapMode where
  pretty' NoOverlap {}    = undefined
  pretty' Overlappable {} = undefined
  pretty' Overlapping {}  = string "{-# OVERLAPPING #-}"
  pretty' Overlaps {}     = undefined
  pretty' Incoherent {}   = undefined

instance Pretty StringLiteral where
  pretty' = output

-- | This instance is for type family declarations inside a class declaration.
instance Pretty (FamilyDecl GhcPs) where
  pretty' FamilyDecl {..} = do
    string "type "
    pretty fdLName
    spacePrefixed $ pretty <$> hsq_explicit fdTyVars
    string " = "
    pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      pretty x

instance Pretty DeclTypeFamily where
  pretty' (DeclTypeFamily FamilyDecl {..}) = do
    string $
      case fdInfo of
        DataFamily          -> "data"
        OpenTypeFamily      -> "type"
        ClosedTypeFamily {} -> "type"
    string " family "
    pretty fdLName
    spacePrefixed $ pretty <$> hsq_explicit fdTyVars
    case unLoc fdResultSig of
      NoSig {} -> pure ()
      TyVarSig {} -> do
        string " = "
        pretty fdResultSig
      _ -> do
        space
        pretty fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      pretty x
    case fdInfo of
      ClosedTypeFamily (Just xs) -> do
        string " where"
        newline
        indentedBlock $ lined $ fmap pretty xs
      _ -> pure ()

instance Pretty (FamilyResultSig GhcPs) where
  pretty' NoSig {} = pure ()
  pretty' (KindSig _ x) = do
    string ":: "
    pretty x
  pretty' (TyVarSig _ x) = pretty x

instance Pretty (HsTyVarBndr a GhcPs) where
  pretty' (UserTyVar _ _ x) = pretty x
  pretty' (KindedTyVar _ _ name ty) =
    parens $ do
      pretty name
      string " :: "
      pretty ty

instance Pretty (InjectivityAnn GhcPs) where
  pretty' (InjectivityAnn _ from to) = do
    pretty from
    string " -> "
    spaced $ fmap pretty to

instance Pretty (ArithSeqInfo GhcPs) where
  pretty' (From from) =
    brackets $ do
      pretty from
      string " .."
  pretty' FromThen {} = undefined
  pretty' (FromTo from to) =
    brackets $ do
      pretty from
      string " .. "
      pretty to
  pretty' FromThenTo {} = undefined

instance Pretty (HsForAllTelescope GhcPs) where
  pretty' HsForAllVis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_vis_bndrs
    dot
  pretty' HsForAllInvis {..} = do
    string "forall"
    forM_ hsf_invis_bndrs $ \case
      (L l (UserTyVar _ SpecifiedSpec ty)) -> space >> pretty (L l ty)
      (L l (UserTyVar _ InferredSpec ty))  -> space >> pretty (L l ty)
      _                                    -> undefined
    dot

instance Pretty InfixOp where
  pretty' (InfixOp (Unqual name)) = backticksIfNotSymbol name $ pretty name
  pretty' (InfixOp (Qual modName name)) =
    backticksIfNotSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (InfixOp Orig {}) = undefined
  pretty' (InfixOp (Exact name)) = backticksIfNotSymbol occ $ pretty occ
    where
      occ = occName name

instance Pretty PrefixOp where
  pretty' (PrefixOp (Unqual name)) = parensIfSymbol name $ pretty name
  pretty' (PrefixOp (Qual modName name)) =
    parensIfSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (PrefixOp Orig {}) = undefined
  pretty' (PrefixOp (Exact name)) = parensIfSymbol occ $ pretty occ
    where
      occ = occName name
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty Context where
  pretty' (Context xs) =
    pretty (HorizontalContext xs) <-|> pretty (VerticalContext xs)

instance Pretty HorizontalContext where
  pretty' (HorizontalContext xs) =
    constraintsParens $ printCommentsAnd xs (hCommaSep . fmap pretty)
    where
      constraintsParens =
        case xs of
          (L _ [])  -> parens
          (L _ [_]) -> id
          _         -> parens

instance Pretty VerticalContext where
  pretty' (VerticalContext (L _ [])) = undefined
  pretty' (VerticalContext full@(L _ [x])) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext xs) = printCommentsAnd xs (vTuple . fmap pretty)
#else
instance Pretty Context where
  pretty' (Context xs) =
    pretty (HorizontalContext xs) <-|> pretty (VerticalContext xs)

instance Pretty HorizontalContext where
  pretty' (HorizontalContext xs) =
    constraintsParens $ mapM_ (`printCommentsAnd` (hCommaSep . fmap pretty)) xs
    where
      constraintsParens =
        case xs of
          Nothing        -> id
          Just (L _ [])  -> parens
          Just (L _ [_]) -> id
          Just _         -> parens

instance Pretty VerticalContext where
  pretty' (VerticalContext Nothing) = undefined
  pretty' (VerticalContext (Just (L _ []))) = undefined
  pretty' (VerticalContext (Just full@(L _ [x]))) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext (Just xs)) =
    printCommentsAnd xs (vTuple . fmap pretty)
#endif
-- Wrap a value of this type with 'ModulenameWithPrefix' to print it with
-- the "module " prefix.
instance Pretty ModuleName where
  pretty' = output

instance Pretty ModuleNameWithPrefix where
  pretty' (ModuleNameWithPrefix name) = spaced [string "module", pretty name]

instance Pretty (IE GhcPs) where
  pretty' (IEVar _ name) = pretty name
  pretty' (IEThingAbs _ name) = pretty name
  pretty' (IEThingAll _ name) = do
    pretty name
    string "(..)"
  -- FIXME: Currently, pretty-printing a 'IEThingWith' uses
  -- 'ghc-lib-parser''s pretty-printer. However, we should avoid it because
  -- 'ghc-lib-parser' may suddenly change how it prints, resulting in
  -- unexpected test failures.
  pretty' x@IEThingWith {} =
    case lines $ showOutputable x of
      [] -> pure ()
      [x'] -> string x'
      xs -> do
        string $ head xs
        indentedWithFixedLevel 0 $ newlinePrefixed $ string <$> tail xs
  pretty' (IEModuleContents _ name) = pretty $ fmap ModuleNameWithPrefix name
  pretty' IEGroup {} = undefined
  pretty' IEDoc {} = undefined
  pretty' IEDocNamed {} = undefined

instance Pretty (FamEqn GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' FamEqn {..} = do
    pretty feqn_tycon
    spacePrefixed $ fmap pretty feqn_pats
    string " = "
    pretty feqn_rhs

-- | Pretty-print a 'data instance'.
instance Pretty (FamEqn GhcPs (HsDataDefn GhcPs))
  -- Current implementation adds two spaces after 'data instances'.
  -- This is intentional but needs to be fixed.
                                                where
  pretty' FamEqn {..} =
    spaced $
    [string "data instance ", pretty feqn_tycon] ++
    fmap pretty feqn_pats ++
    [string "=", pretty $ HsDataDefnForDataInstance feqn_rhs]

-- HsArg (LHsType GhcPs) (LHsType GhcPs)
instance Pretty (HsArg (GenLocated SrcSpanAnnA (HsType GhcPs)) (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' (HsValArg x) = pretty x
  pretty' HsTypeArg {} = undefined
  pretty' HsArgPar {}  = undefined
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsQuote GhcPs) where
  pretty' (ExpBr _ x) = brackets $ wrapWithBars $ pretty x
  pretty' (PatBr _ x) =
    brackets $ do
      string "p"
      wrapWithBars $ pretty x
  pretty' DecBrL {} = undefined
  pretty' DecBrG {} = undefined
  pretty' (TypBr _ x) =
    brackets $ do
      string "t"
      wrapWithBars $ pretty x
  pretty' (VarBr _ isSingle x) = do
    if isSingle
      then string "'"
      else string "''"
    pretty x
#endif
instance Pretty (WarnDecls GhcPs) where
  pretty' (Warnings _ _ x) = lined $ fmap pretty x

instance Pretty (WarnDecl GhcPs) where
  pretty' (Warning _ names (DeprecatedTxt _ reasons)) = do
    string "{-# DEPRECATED"
    newline
    hCommaSep $ fmap pretty names
    space
    hCommaSep $ fmap pretty reasons
    newline
    string " #-}"
  pretty' _ = undefined
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (WithHsDocIdentifiers StringLiteral GhcPs) where
  pretty' WithHsDocIdentifiers {..} = pretty hsDocString
#endif
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (IEWrappedName RdrName) where
  pretty' (IEName name) = pretty name
  pretty' IEPattern {} = undefined
  pretty' (IEType _ name) = do
    string "type "
    pretty name
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (DotFieldOcc GhcPs) where
  pretty' DotFieldOcc {..} = pretty dfoLabel
#else
instance Pretty (HsFieldLabel GhcPs) where
  pretty' HsFieldLabel {..} = pretty hflLabel
#endif
instance Pretty FastString where
  pretty' = output

instance Pretty (RuleDecls GhcPs) where
  pretty' HsRules {..} =
    lined $ [string "{-# RULES"] ++ fmap pretty rds_rules ++ [string " #-}"]

instance Pretty (RuleDecl GhcPs) where
  pretty' HsRule {..} =
    spaced
      [ printCommentsAnd rd_name (doubleQuotes . pretty . snd)
      , pretty rd_lhs
      , string "="
      , pretty rd_rhs
      ]

instance Pretty OccName where
  pretty' = output

instance Pretty (DerivDecl GhcPs)
  -- TODO: Handle deriving strategies.
                                       where
  pretty' DerivDecl {..} = do
    string "deriving instance "
    pretty deriv_type

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance Pretty (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsSigType GhcPs))) where
  pretty' HsWC {..} = pretty hswc_body

-- | 'Pretty' for 'LHsWcType'
instance Pretty (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' HsWC {..} = pretty hswc_body

instance Pretty (StandaloneKindSig GhcPs) where
  pretty' (StandaloneKindSig _ name kind) =
    spaced [string "type", pretty name, string "::", pretty kind]

instance Pretty (DefaultDecl GhcPs) where
  pretty' _ = undefined

instance Pretty (ForeignDecl GhcPs)
  -- TODO: Implement correctly.
                                where
  pretty' ForeignImport {fd_fi = (CImport _ safety _ _ _)} = do
    string "foreign import ccall "
    pretty safety
    string " \"test\" test :: IO ()"
  pretty' ForeignExport {} =
    string "foreign export ccall \"test\" test :: IO ()"

instance Pretty Safety where
  pretty' PlaySafe          = string "safe"
  pretty' PlayInterruptible = undefined
  pretty' PlayRisky         = string "unsafe"

instance Pretty (AnnDecl GhcPs) where
  pretty' (HsAnnotation _ _ prov expr) =
    spaced [string "{-# ANN", pretty prov, pretty expr, string "#-}"]

instance Pretty (AnnProvenance GhcPs) where
  pretty' (ValueAnnProvenance x) = pretty x
  pretty' TypeAnnProvenance {}   = undefined
  pretty' ModuleAnnProvenance {} = undefined

instance Pretty (RoleAnnotDecl GhcPs) where
  pretty' (RoleAnnotDecl _ name roles) =
    spaced $
    [string "type role", pretty name] ++
    fmap (maybe (string "_") pretty . unLoc) roles

instance Pretty Role where
  pretty' Nominal          = undefined
  pretty' Representational = string "representational"
  pretty' Phantom          = undefined

instance Pretty (TyFamInstDecl GhcPs) where
  pretty' TyFamInstDecl {..} = do
    string "type instance "
    pretty tfid_eqn

instance Pretty (DataFamInstDecl GhcPs) where
  pretty' DataFamInstDecl {..} = pretty dfid_eqn

instance Pretty (PatSynBind GhcPs GhcPs) where
  pretty' PSB {..} =
    spaced
      [ string "pattern"
      , pretty psb_id
      , pretty psb_args
      , pretty psb_dir
      , pretty psb_def
      ]

-- | 'Pretty' for 'HsPatSynDetails'.
instance Pretty (HsConDetails Void (GenLocated SrcSpanAnnN RdrName) [RecordPatSynField GhcPs]) where
  pretty' (PrefixCon _ xs) = spaced $ fmap pretty xs
  pretty' RecCon {}        = undefined
  pretty' InfixCon {}      = undefined

instance Pretty (FixitySig GhcPs) where
  pretty' (FixitySig _ names fixity) =
    spaced [pretty fixity, hCommaSep $ fmap (pretty . fmap InfixOp) names]

instance Pretty Fixity where
  pretty' (Fixity _ level dir) = spaced [pretty dir, string $ show level]

instance Pretty FixityDirection where
  pretty' InfixL = string "infixl"
  pretty' InfixR = undefined
  pretty' InfixN = undefined

instance Pretty InlinePragma where
  pretty' InlinePragma {..} = pretty inl_inline

instance Pretty InlineSpec where
  pretty' = prettyInlineSpec

prettyInlineSpec :: InlineSpec -> Printer ()
prettyInlineSpec Inline {}        = string "INLINE"
prettyInlineSpec Inlinable {}     = undefined
prettyInlineSpec NoInline {}      = string "NOINLINE"
prettyInlineSpec NoUserInlinePrag = undefined
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyInlineSpec Opaque {}        = undefined
#endif
instance Pretty (HsPatSynDir GhcPs) where
  pretty' Unidirectional           = string "<-"
  pretty' ImplicitBidirectional    = string "="
  pretty' ExplicitBidirectional {} = undefined

instance Pretty (HsOverLit GhcPs) where
  pretty' OverLit {..} = pretty ol_val

instance Pretty OverLitVal where
  pretty' (HsIntegral x)   = pretty x
  pretty' (HsFractional x) = pretty x
  pretty' (HsIsString _ x) = pretty x

instance Pretty IntegralLit where
  pretty' IL {..} = string $ show il_value

instance Pretty FractionalLit where
  pretty' = output

instance Pretty (HsLit GhcPs) where
  pretty' x@(HsChar _ _)   = output x
  pretty' HsCharPrim {}    = undefined
  pretty' x@(HsString _ _) = output x
  pretty' HsStringPrim {}  = undefined
  pretty' HsInt {}         = undefined
  pretty' HsIntPrim {}     = undefined
  pretty' HsWordPrim {}    = undefined
  pretty' HsInt64Prim {}   = undefined
  pretty' HsWord64Prim {}  = undefined
  pretty' HsInteger {}     = undefined
  pretty' HsRat {}         = undefined
  pretty' HsFloatPrim {}   = undefined
  pretty' HsDoublePrim {}  = undefined

instance Pretty (HsPragE GhcPs) where
  pretty' (HsPragSCC _ _ x) = spaced [string "{-# SCC", pretty x, string "#-}"]

instance Pretty HsIPName where
  pretty' (HsIPName x) = pretty x

instance Pretty HsTyLit where
  pretty' HsNumTy {}    = undefined
  pretty' (HsStrTy _ x) = string $ show x
  pretty' HsCharTy {}   = undefined

instance Pretty (HsPatSigType GhcPs) where
  pretty' HsPS {..} = pretty hsps_body

instance Pretty (HsIPBinds GhcPs) where
  pretty' (IPBinds _ xs) = lined $ fmap pretty xs

instance Pretty (IPBind GhcPs) where
  pretty' = prettyIPBind

prettyIPBind :: IPBind GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyIPBind (IPBind _ l r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#else
prettyIPBind (IPBind _ (Right _) _) = undefined
prettyIPBind (IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#endif
instance Pretty (DerivStrategy GhcPs) where
  pretty' StockStrategy {}    = undefined
  pretty' AnyclassStrategy {} = string "anyclass"
  pretty' NewtypeStrategy {}  = string "newtype"
  pretty' ViaStrategy {}      = undefined
