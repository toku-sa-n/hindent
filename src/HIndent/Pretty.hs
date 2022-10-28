{-# LANGUAGE CPP                       #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE LambdaCase                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE ViewPatterns              #-}

-- | Pretty printing.
--
-- Some instances define top-level functions to handle CPP.
--
-- Some value constructors never appear in an AST. GHC has three stages for
-- using an AST: parsing, renaming, and type checking, and GHC uses these
-- constructors only in remaining and type checking.
module HIndent.Pretty
  ( pretty
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.List
import           Data.Maybe
import           Data.Void
import           GHC.Core.Coercion
import           GHC.Core.InstEnv
import           GHC.Data.Bag
import           GHC.Data.BooleanFormula
import           GHC.Data.FastString
import           GHC.Hs
import           GHC.Stack
import           GHC.Types.Basic
import           GHC.Types.Fixity
import           GHC.Types.ForeignCall
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           GHC.Types.SourceText
import           GHC.Types.SrcLoc
import           GHC.Unit
import           GHC.Unit.Module.Warnings
import           HIndent.Applicative
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Import
import           HIndent.Pretty.Pragma
import           HIndent.Pretty.SigBindFamily
import           HIndent.Pretty.Types
import           HIndent.Types
#if MIN_VERSION_ghc_lib_parser(9,4,1)
import           GHC.Types.PkgQual
#endif
-- | A wrapper of an AST node from which comments can be extracted.
data CommentExtractable =
  forall a. Pretty a =>
            CommentExtractable a

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
-- 'FastString' does not implement this class because it may contain @\n@s
-- and each type that may contain a 'FastString' value needs their own
-- handlings.
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
  {-# MINIMAL pretty'
            , (commentsFrom | (commentsBefore, commentOnSameLine, commentsAfter)) #-}
  pretty' :: a -> Printer ()
  -- | Returns an AST node from which comments of @a@ can be extracted.
  --
  -- This function must not return an AST node that has comments of @a@'s
  -- children.
  commentsFrom :: a -> Maybe CommentExtractable
  commentsFrom = const Nothing
  -- | Returns comments that are before the given AST node.
  commentsBefore :: a -> [LEpaComment]
  commentsBefore (commentsFrom -> Just (CommentExtractable x)) =
    commentsBefore x
  commentsBefore _ = []
  -- | Returns a comment that is on the same line as the last line of the given AST node if it exists.
  commentOnSameLine :: a -> Maybe LEpaComment
  commentOnSameLine (commentsFrom -> Just (CommentExtractable x)) =
    commentOnSameLine x
  commentOnSameLine _ = Nothing
  -- | Returns comments that are after the given AST node.
  commentsAfter :: a -> [LEpaComment]
  commentsAfter (commentsFrom -> Just (CommentExtractable x)) = commentsAfter x
  commentsAfter _                                             = []

instance Pretty HsModule where
  pretty' m = blanklined printers >> newline
    -- TODO: Refactor this 'where' clause.
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists m, prettyPragmas m)
        , (moduleDeclExists m, prettyModuleDecl m)
        , (importsExist m, prettyImports)
        , (declsExist m, prettyDecls)
        ]
      prettyModuleDecl HsModule {hsmodName = Nothing} =
        error "The module declaration does not exist."
      prettyModuleDecl HsModule {hsmodName = Just name, hsmodExports = Nothing} = do
        pretty $ fmap ModuleNameWithPrefix name
        string " where"
      prettyModuleDecl HsModule { hsmodName = Just name
                                , hsmodExports = Just exports
                                } = do
        pretty $ fmap ModuleNameWithPrefix name
        newline
        indentedBlock $ do
          printCommentsAnd exports (vTuple . fmap pretty)
          string " where"
      moduleDeclExists HsModule {hsmodName = Nothing} = False
      moduleDeclExists _                              = True
      prettyDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addDeclSeparator $ hsmodDecls m
      addDeclSeparator [] = []
      addDeclSeparator [x] = [(x, Nothing)]
      addDeclSeparator (x:xs) =
        (x, Just $ declSeparator $ unLoc x) : addDeclSeparator xs
      declSeparator (SigD _ TypeSig {})   = newline
      declSeparator (SigD _ InlineSig {}) = newline
      declSeparator _                     = blankline
      declsExist = not . null . hsmodDecls
      prettyImports = importDecls >>= blanklined . fmap outputImportGroup
      outputImportGroup = lined . fmap pretty
      importDecls =
        gets (configSortImports . psConfig) >>= \case
          True  -> pure $ extractImportsSorted m
          False -> pure $ extractImports m
  commentsBefore =
    filter isNeitherEofNorPragmaComment . priorComments . comments . hsmodAnn
    where
      isNeitherEofNorPragmaComment (L _ (EpaComment EpaEofComment _)) = False
      isNeitherEofNorPragmaComment (L _ (EpaComment tok _)) = not $ isPragma tok
  commentOnSameLine = const Nothing
  commentsAfter =
    filter (not . isPragma . ac_tok . unLoc) .
    followingComments . comments . hsmodAnn

-- FIXME: Requiring 'l' to implement 'Pretty' is wrong because some types
-- (e.g., 'EpAnn') cannot pretty-print. The restriction exists only for
-- extracting comments. Remove the restriction.
instance (Pretty l, Pretty e) => Pretty (GenLocated l e) where
  pretty' (L _ e) = pretty e
  commentsFrom (L l _) = Just $ CommentExtractable l

instance Pretty (HsDecl GhcPs) where
  pretty' (TyClD _ d) = pretty d
  pretty' (InstD _ inst) = pretty inst
  pretty' (DerivD _ x) = pretty x
  pretty' (ValD _ bind) = pretty bind
  pretty' (SigD _ s) = pretty $ DeclSig s
  pretty' (KindSigD _ x) = pretty x
  pretty' (DefD _ x) = pretty x
  pretty' (ForD _ x) = pretty x
  pretty' (WarningD _ x) = pretty x
  pretty' (AnnD _ x) = pretty x
  pretty' (RuleD _ x) = pretty x
  pretty' (SpliceD _ sp) = pretty sp
  pretty' DocD {} = error "Document comments should be treated as normal ones."
  pretty' (RoleAnnotD _ x) = pretty x
  commentsFrom TyClD {} = Nothing
  commentsFrom InstD {} = Nothing
  commentsFrom DerivD {} = Nothing
  commentsFrom ValD {} = Nothing
  commentsFrom SigD {} = Nothing
  commentsFrom KindSigD {} = Nothing
  commentsFrom DefD {} = Nothing
  commentsFrom ForD {} = Nothing
  commentsFrom WarningD {} = Nothing
  commentsFrom AnnD {} = Nothing
  commentsFrom RuleD {} = Nothing
  commentsFrom SpliceD {} = Nothing
  commentsFrom DocD {} =
    error "Document comments should be treated as normal ones."
  commentsFrom RoleAnnotD {} = Nothing

instance Pretty (TyClDecl GhcPs) where
  pretty' = prettyTyClDecl
  commentsFrom FamDecl {} = Nothing
  commentsFrom SynDecl {..} = Just $ CommentExtractable tcdSExt
  commentsFrom DataDecl {..} = Just $ CommentExtractable tcdDExt
  commentsFrom ClassDecl {tcdCExt = (x, _, _)} = Just $ CommentExtractable x

prettyTyClDecl :: TyClDecl GhcPs -> Printer ()
prettyTyClDecl (FamDecl _ x) = pretty x
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
            xs  -> hvTuple $ fmap pretty xs
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
  commentsFrom = commentsFromInstDecl

commentsFromInstDecl :: InstDecl GhcPs -> Maybe CommentExtractable
commentsFromInstDecl ClsInstD {}       = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromInstDecl DataFamInstD {}   = Nothing
#else
commentsFromInstDecl DataFamInstD {..} = Just $ CommentExtractable dfid_ext
#endif
commentsFromInstDecl TyFamInstD {}     = Nothing

instance Pretty (HsBind GhcPs) where
  pretty' = prettyHsBind
  commentsFrom = commentsFromHsBind

prettyHsBind :: HsBind GhcPs -> Printer ()
prettyHsBind FunBind {..} = pretty fun_matches
prettyHsBind PatBind {..} = do
  pretty pat_lhs
  pretty pat_rhs
prettyHsBind VarBind {} = notUsedInParsedStage
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsBind AbsBinds {} = notUsedInParsedStage
#endif
prettyHsBind (PatSynBind _ x) = pretty x

commentsFromHsBind :: HsBind GhcPs -> Maybe CommentExtractable
commentsFromHsBind FunBind {..}  = Just $ CommentExtractable fun_id
commentsFromHsBind PatBind {..}  = Just $ CommentExtractable pat_ext
commentsFromHsBind VarBind {}    = Nothing
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsBind AbsBinds {}   = Nothing
#endif
commentsFromHsBind PatSynBind {} = Nothing

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
  pretty' IdSig {} = notUsedInParsedStage
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
  commentsFrom (TypeSig x _ _)            = Just $ CommentExtractable x
  commentsFrom (PatSynSig x _ _)          = Just $ CommentExtractable x
  commentsFrom (ClassOpSig x _ _ _)       = Just $ CommentExtractable x
  commentsFrom IdSig {}                   = Nothing
  commentsFrom (FixSig x _)               = Just $ CommentExtractable x
  commentsFrom (InlineSig x _ _)          = Just $ CommentExtractable x
  commentsFrom (SpecSig x _ _ _)          = Just $ CommentExtractable x
  commentsFrom (SpecInstSig x _ _)        = Just $ CommentExtractable x
  commentsFrom (MinimalSig x _ _)         = Just $ CommentExtractable x
  commentsFrom (SCCFunSig x _ _ _)        = Just $ CommentExtractable x
  commentsFrom (CompleteMatchSig x _ _ _) = Just $ CommentExtractable x

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
  commentsFrom (DeclSig x) = Just $ CommentExtractable x

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
  commentsFrom HsDataDefn {} = Nothing

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
  commentsFrom ClsInstDecl {cid_ext = (x, _)} = Just $ CommentExtractable x

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' MG {..} = printCommentsAnd mg_alts (lined . fmap pretty)
  commentsFrom MG {} = Nothing

instance Pretty MatchGroupForCase where
  pretty' (MatchGroupForCase MG {..}) =
    printCommentsAnd mg_alts (lined . fmap (pretty . fmap MatchForCase))
  commentsFrom (MatchGroupForCase x) = Just $ CommentExtractable x

instance Pretty MatchGroupForLambda where
  pretty' (MatchGroupForLambda MG {..}) =
    printCommentsAnd mg_alts (lined . fmap (pretty . fmap MatchForLambda))
  commentsFrom (MatchGroupForLambda x) = Just $ CommentExtractable x

instance Pretty MatchGroupForLambdaInProc where
  pretty' (MatchGroupForLambdaInProc MG {..}) =
    printCommentsAnd mg_alts (lined . fmap (pretty . fmap MatchForLambdaInProc))
  commentsFrom (MatchGroupForLambdaInProc MG {}) = Nothing

instance Pretty MatchGroupForCaseInProc where
  pretty' (MatchGroupForCaseInProc MG {..}) =
    printCommentsAnd mg_alts (lined . fmap (pretty . fmap MatchForCaseInProc))
  commentsFrom (MatchGroupForCaseInProc MG {}) = Nothing

instance Pretty (HsExpr GhcPs) where
  pretty' = prettyHsExpr
  commentsFrom = commentsFromHsExpr

prettyHsExpr :: HsExpr GhcPs -> Printer ()
prettyHsExpr (HsVar _ bind) = pretty $ fmap PrefixOp bind
prettyHsExpr (HsUnboundVar _ x) = pretty x
prettyHsExpr (HsOverLabel _ l) = do
  string "#"
  string $ unpackFS l
prettyHsExpr (HsIPVar _ var) = do
  string "?"
  pretty var
prettyHsExpr (HsOverLit _ x) = pretty x
prettyHsExpr (HsLit _ l) = pretty l
prettyHsExpr (HsLam _ body) = pretty $ MatchGroupForLambda body
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLamCase _ _ matches) = pretty $ LambdaCase matches
#else
prettyHsExpr (HsLamCase _ matches) = pretty $ LambdaCase matches
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
prettyHsExpr (ExplicitSum _ _ _ expr) =
  unboxedSums $ spaced [string "|", pretty expr]
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
        (L _ (HsDo _ MDoExpr {} xs)) -> do
          string str
          string "mdo"
          newline
          indentedBlock $ printCommentsAnd xs (lined . fmap pretty)
        _ -> string str |=> pretty e
prettyHsExpr (HsMultiIf _ guards) =
  string "if " |=> lined (fmap (pretty . fmap GRHSForMultiwayIf) guards)
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr (HsLet _ _ binds _ exprs) = pretty $ LetIn binds exprs
#else
prettyHsExpr (HsLet _ binds exprs) = pretty $ LetIn binds exprs
#endif
prettyHsExpr (HsDo _ ListComp {} (L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (HsDo _ ListComp {} (L l (lhs:rhs))) =
  pretty $ L l $ ListComprehension lhs rhs
-- While the name contains 'Monad', 'MonadComp' is for list comprehensions.
prettyHsExpr (HsDo _ MonadComp {} (L _ [])) =
  error "Not enough arguments are passed to pretty-print a list comprehension."
prettyHsExpr (HsDo _ MonadComp {} (L l (lhs:rhs))) =
  pretty $ L l $ ListComprehension lhs rhs
prettyHsExpr (HsDo _ DoExpr {} (L l xs)) = pretty $ L l $ DoExpression xs Do
prettyHsExpr (HsDo _ MDoExpr {} (L l xs)) = pretty $ L l $ DoExpression xs Mdo
prettyHsExpr (HsDo _ GhciStmtCtxt {} _) = error "We're not using GHCi, are we?"
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
prettyHsExpr HsProjection {..} =
  parens $
  forM_ proj_flds $ \x -> do
    string "."
    pretty x
prettyHsExpr (ExprWithTySig _ e sig) = do
  pretty e
  string " :: "
  pretty $ hswc_body sig
prettyHsExpr (ArithSeq _ _ x) = pretty x
prettyHsExpr (HsSpliceE _ x) = pretty x
-- TODO: Handle comments.
--
-- TODO: Maybe we should move these definitions inside the @instance Pretty
-- (HsCmdTop GhcPs)@?
prettyHsExpr (HsProc _ pat x@(L _ (HsCmdTop _ (L _ (HsCmdDo _ xs))))) = do
  spaced [string "proc", pretty pat, string "-> do"]
  newline
  indentedBlock $
    printCommentsAnd x (const (printCommentsAnd xs (lined . fmap pretty)))
prettyHsExpr (HsProc _ pat body) = hor <-|> ver
  where
    hor = spaced [string "proc", pretty pat, string "->", pretty body]
    ver = do
      spaced [string "proc", pretty pat, string "->"]
      newline
      indentedBlock (pretty body)
prettyHsExpr (HsStatic _ x) = spaced [string "static", pretty x]
prettyHsExpr (HsPragE _ p x) = spaced [pretty p, pretty x]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsExpr HsRecSel {} = notUsedInParsedStage
prettyHsExpr (HsTypedBracket _ inner) = typedBrackets $ pretty inner
prettyHsExpr (HsUntypedBracket _ inner) = pretty inner
#else
prettyHsExpr HsConLikeOut {} = notUsedInParsedStage
prettyHsExpr HsRecFld {} = notUsedInParsedStage
prettyHsExpr (HsDo _ ArrowExpr {} _) = notUsedInParsedStage
prettyHsExpr (HsDo _ PatGuard {} _) = error "PatGuard should never appear here."
prettyHsExpr (HsDo _ ParStmtCtxt {} _) =
  error "ParStmtCtxt should never appear here."
prettyHsExpr (HsDo _ TransStmtCtxt {} _) = notUsedInParsedStage
prettyHsExpr HsTick {} = forHpc
prettyHsExpr HsBinTick {} = forHpc
prettyHsExpr (HsBracket _ inner) = pretty inner
prettyHsExpr HsRnBracketOut {} = notUsedInParsedStage
prettyHsExpr HsTcBracketOut {} = notUsedInParsedStage
#endif
instance Pretty LambdaCase where
  pretty' (LambdaCase matches) = do
    string "\\case"
    if null $ unLoc $ mg_alts matches
      then string " {}"
      else do
        newline
        indentedBlock $ pretty $ MatchGroupForCase matches
  commentsFrom (LambdaCase x) = Just $ CommentExtractable x

commentsFromHsExpr :: HsExpr GhcPs -> Maybe CommentExtractable
commentsFromHsExpr HsVar {}               = Nothing
commentsFromHsExpr (HsUnboundVar x _)     = Just $ CommentExtractable x
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr HsConLikeOut {}        = Nothing
commentsFromHsExpr HsRecFld {}            = Nothing
#endif
commentsFromHsExpr (HsOverLabel x _)      = Just $ CommentExtractable x
commentsFromHsExpr (HsIPVar x _)          = Just $ CommentExtractable x
commentsFromHsExpr (HsOverLit x _)        = Just $ CommentExtractable x
commentsFromHsExpr (HsLit x _)            = Just $ CommentExtractable x
commentsFromHsExpr HsLam {}               = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr (HsLamCase x _ _)      = Just $ CommentExtractable x
#else
commentsFromHsExpr (HsLamCase x _)        = Just $ CommentExtractable x
#endif
commentsFromHsExpr (HsApp x _ _)          = Just $ CommentExtractable x
commentsFromHsExpr HsAppType {}           = Nothing
commentsFromHsExpr (OpApp x _ _ _)        = Just $ CommentExtractable x
commentsFromHsExpr (NegApp x _ _)         = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr (HsPar x _ _ _)        = Just $ CommentExtractable x
#else
commentsFromHsExpr (HsPar x _)            = Just $ CommentExtractable x
#endif
commentsFromHsExpr (SectionL x _ _)       = Just $ CommentExtractable x
commentsFromHsExpr (SectionR x _ _)       = Just $ CommentExtractable x
commentsFromHsExpr (ExplicitTuple x _ _)  = Just $ CommentExtractable x
commentsFromHsExpr (ExplicitSum x _ _ _)  = Just $ CommentExtractable x
commentsFromHsExpr (HsCase x _ _)         = Just $ CommentExtractable x
commentsFromHsExpr (HsIf x _ _ _)         = Just $ CommentExtractable x
commentsFromHsExpr (HsMultiIf x _)        = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr (HsLet x _ _ _ _)      = Just $ CommentExtractable x
#else
commentsFromHsExpr (HsLet x _ _)          = Just $ CommentExtractable x
#endif
commentsFromHsExpr (HsDo x _ _)           = Just $ CommentExtractable x
commentsFromHsExpr (ExplicitList x _)     = Just $ CommentExtractable x
commentsFromHsExpr RecordCon {..}         = Just $ CommentExtractable rcon_ext
commentsFromHsExpr RecordUpd {..}         = Just $ CommentExtractable rupd_ext
commentsFromHsExpr HsGetField {..}        = Just $ CommentExtractable gf_ext
commentsFromHsExpr HsProjection {..}      = Just $ CommentExtractable proj_ext
commentsFromHsExpr (ExprWithTySig x _ _)  = Just $ CommentExtractable x
commentsFromHsExpr (ArithSeq x _ _)       = Just $ CommentExtractable x
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr (HsBracket x _)        = Just $ CommentExtractable x
commentsFromHsExpr HsRnBracketOut {}      = notUsedInParsedStage
commentsFromHsExpr HsTcBracketOut {}      = notUsedInParsedStage
#endif
commentsFromHsExpr (HsSpliceE x _)        = Just $ CommentExtractable x
commentsFromHsExpr (HsProc x _ _)         = Just $ CommentExtractable x
commentsFromHsExpr (HsStatic x _)         = Just $ CommentExtractable x
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr HsTick {}              = Nothing
commentsFromHsExpr HsBinTick {}           = Nothing
#endif
commentsFromHsExpr HsPragE {}             = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsExpr HsRecSel {}            = Nothing
commentsFromHsExpr (HsTypedBracket x _)   = Just $ CommentExtractable x
commentsFromHsExpr (HsUntypedBracket x _) = Just $ CommentExtractable x
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
  commentsFrom HsSig {} = Nothing

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
  commentsFrom (HsSigTypeInsideInstDecl x) = Just $ CommentExtractable x
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
  commentsFrom (HsSigTypeInsideVerticalFuncSig x) = Just $ CommentExtractable x

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
  commentsFrom (HsSigTypeInsideDeclSig x) = Just $ CommentExtractable x
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
  commentsFrom (HsSigTypeInsideVerticalFuncSig x) = Just $ CommentExtractable x

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
  commentsFrom (HsSigTypeInsideDeclSig x) = Just $ CommentExtractable x
#endif
instance Pretty (ConDecl GhcPs) where
  pretty' = prettyConDecl
  commentsFrom ConDeclGADT {..} = Just $ CommentExtractable con_g_ext
  commentsFrom ConDeclH98 {..}  = Just $ CommentExtractable con_ext

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
  commentsFrom Match {..} = Just $ CommentExtractable m_ext

instance Pretty MatchForCase where
  pretty' (MatchForCase Match {..}) = do
    mapM_ pretty m_pats
    pretty (GRHSsForCase m_grhss)
  commentsFrom (MatchForCase x) = Just $ CommentExtractable x

instance Pretty MatchForLambda where
  pretty' (MatchForLambda Match {..}) = do
    string "\\"
    unless (null m_pats) $
      case unLoc $ head m_pats of
        LazyPat {} -> space
        BangPat {} -> space
        _          -> return ()
    spaced $ fmap pretty m_pats ++ [pretty $ GRHSsForLambda m_grhss]
  commentsFrom (MatchForLambda x) = Just $ CommentExtractable x

instance Pretty MatchForLambdaInProc where
  pretty' (MatchForLambdaInProc Match {..}) = do
    string "\\"
    unless (null m_pats) $
      case unLoc $ head m_pats of
        LazyPat {} -> space
        BangPat {} -> space
        _          -> return ()
    spaced $ fmap pretty m_pats ++ [pretty $ GRHSsForLambdaInProc m_grhss]
  commentsFrom (MatchForLambdaInProc Match {..}) =
    Just $ CommentExtractable m_ext

instance Pretty MatchForCaseInProc where
  pretty' (MatchForCaseInProc Match {..}) = do
    mapM_ pretty m_pats
    pretty (GRHSsForCaseInProc m_grhss)
  commentsFrom (MatchForCaseInProc Match {..}) = Just $ CommentExtractable m_ext

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
  pretty' ApplicativeStmt {} = notUsedInParsedStage
  pretty' (BodyStmt _ (L loc (OpApp _ l o r)) _ _) =
    pretty (L loc (InfixApp l o r True))
  pretty' (BodyStmt _ body _ _) = pretty body
  pretty' (LetStmt _ l) = string "let " |=> pretty l
  pretty' (ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)
  commentsFrom (LetStmt l _) = Just $ CommentExtractable l
  commentsFrom _             = Nothing

instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsCmd GhcPs))) where
  pretty' (LastStmt _ x _ _) = pretty x
  pretty' (BindStmt _ pat body) = hor <-|> ver
    where
      hor = spaced [pretty pat, string "<-", pretty body]
      ver = do
        pretty pat
        string " <-"
        newline
        indentedBlock $ pretty body
  pretty' ApplicativeStmt {} = notUsedInParsedStage
  pretty' (BodyStmt _ body _ _) = pretty body
  pretty' (LetStmt _ l) = string "let " |=> pretty l
  pretty' (ParStmt _ xs _ _) = hvBarSep $ fmap pretty xs
  pretty' TransStmt {..} =
    vCommaSep $ fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {..} =
    string "rec " |=> printCommentsAnd recS_stmts (lined . fmap pretty)
  commentsFrom LastStmt {}        = Nothing
  commentsFrom (BindStmt x _ _)   = Just $ CommentExtractable x
  commentsFrom ApplicativeStmt {} = Nothing
  commentsFrom BodyStmt {}        = Nothing
  commentsFrom (LetStmt x _)      = Just $ CommentExtractable x
  commentsFrom ParStmt {}         = Nothing
  commentsFrom TransStmt {..}     = Just $ CommentExtractable trS_ext
  commentsFrom RecStmt {..}       = Just $ CommentExtractable recS_ext

instance Pretty StmtLRInsideVerticalList where
  pretty' (StmtLRInsideVerticalList (ParStmt _ xs _ _)) =
    vBarSep $ fmap (pretty . ParStmtBlockInsideVerticalList) xs
  pretty' (StmtLRInsideVerticalList x) = pretty x
  commentsFrom (StmtLRInsideVerticalList x) = Just $ CommentExtractable x

-- | For pattern matching.
instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _  -> braces $ string ".."
          Nothing -> hFields $ fmap pretty rec_flds
      vertical = vFields $ fmap pretty rec_flds
  commentsFrom HsRecFields {} = Nothing

-- | For record updates
instance Pretty (HsRecFields GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' HsRecFields {..} = hvFields fieldPrinters
    where
      fieldPrinters =
        fmap pretty rec_flds ++
        maybeToList (fmap (const (string "..")) rec_dotdot)
  commentsFrom HsRecFields {} = Nothing

instance Pretty (HsType GhcPs) where
  pretty' = prettyHsType
  commentsFrom HsForAllTy {}            = Nothing
  commentsFrom HsQualTy {}              = Nothing
  commentsFrom (HsTyVar x _ _)          = Just $ CommentExtractable x
  commentsFrom HsAppTy {}               = Nothing
  commentsFrom HsAppKindTy {}           = Nothing
  commentsFrom (HsFunTy x _ _ _)        = Just $ CommentExtractable x
  commentsFrom (HsListTy x _)           = Just $ CommentExtractable x
  commentsFrom (HsTupleTy x _ _)        = Just $ CommentExtractable x
  commentsFrom (HsSumTy x _)            = Just $ CommentExtractable x
  commentsFrom HsOpTy {}                = Nothing
  commentsFrom (HsParTy x _)            = Just $ CommentExtractable x
  commentsFrom (HsIParamTy x _ _)       = Just $ CommentExtractable x
  commentsFrom HsStarTy {}              = Nothing
  commentsFrom (HsKindSig x _ _)        = Just $ CommentExtractable x
  commentsFrom HsSpliceTy {}            = Nothing
  commentsFrom (HsDocTy x _ _)          = Just $ CommentExtractable x
  commentsFrom (HsBangTy x _ _)         = Just $ CommentExtractable x
  commentsFrom (HsRecTy x _)            = Just $ CommentExtractable x
  commentsFrom (HsExplicitListTy x _ _) = Just $ CommentExtractable x
  commentsFrom (HsExplicitTupleTy x _)  = Just $ CommentExtractable x
  commentsFrom HsTyLit {}               = Nothing
  commentsFrom HsWildCardTy {}          = Nothing
  commentsFrom XHsType {}               = Nothing

prettyHsType :: HsType GhcPs -> Printer ()
prettyHsType (HsForAllTy _ tele body) = (pretty tele >> space) |=> pretty body
prettyHsType HsQualTy {..} = do
  pretty $ Context hst_ctxt
  lined [string " =>", indentedBlock $ pretty hst_body]
prettyHsType (HsTyVar _ NotPromoted x) = pretty x
prettyHsType (HsTyVar _ IsPromoted x) = string "'" >> pretty x
prettyHsType (HsAppTy _ l r) = spaced $ fmap pretty [l, r]
prettyHsType (HsAppKindTy _ l r) = pretty l >> string " @" >> pretty r
prettyHsType (HsFunTy _ _ a b) = hor <-|> noDeclSigV
  where
    hor = spaced [pretty a, string "->", pretty b]
    noDeclSigV = lined [pretty a, prefixed "-> " $ pretty b]
prettyHsType (HsListTy _ xs) = brackets $ pretty xs
prettyHsType (HsTupleTy _ _ xs) = hvTuple' $ fmap pretty xs
prettyHsType (HsSumTy _ xs) = unboxedSums $ hBarSep $ fmap pretty xs
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
prettyHsType HsDocTy {} =
  error
    "An AST node of this type never appears in an AST because haddock comments are treated as normal ones."
prettyHsType (HsBangTy _ _ x) = do
  string "!"
  pretty x
prettyHsType (HsRecTy _ xs) = hvFields $ fmap pretty xs
prettyHsType (HsExplicitListTy _ _ xs) =
  case xs of
    [] -> string "'[]"
    _  -> hPromotedList $ fmap pretty xs
prettyHsType (HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap pretty xs
prettyHsType (HsTyLit _ x) = pretty x
prettyHsType HsWildCardTy {} = string "_"
prettyHsType XHsType {} = notUsedInParsedStage
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
  commentsFrom (HsTypeInsideInstDecl x) = Just $ CommentExtractable x

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
  commentsFrom (HsTypeInsideDeclSig x) = Just $ CommentExtractable x
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
  commentsFrom (HsTypeInsideInstDecl x) = Just $ CommentExtractable x

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
  commentsFrom (HsTypeInsideDeclSig x) = Just $ CommentExtractable x
#endif
instance Pretty HsTypeInsideVerticalFuncSig where
  pretty' (HsTypeInsideVerticalFuncSig (HsFunTy _ _ a b)) = noDeclSigV
    where
      noDeclSigV = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideVerticalFuncSig x) = pretty x
  commentsFrom (HsTypeInsideVerticalFuncSig x) = Just $ CommentExtractable x

instance Pretty HsTypeInsideVerticalDeclSig where
  pretty' (HsTypeInsideVerticalDeclSig (HsFunTy _ _ a b)) = declSigV
    where
      declSigV = do
        pretty $ fmap HsTypeInsideVerticalFuncSig a
        newline
        prefixed "-> " $ pretty $ fmap HsTypeInsideVerticalFuncSig b
  pretty' (HsTypeInsideVerticalDeclSig x) = pretty x
  commentsFrom (HsTypeInsideVerticalDeclSig x) = Just $ CommentExtractable x
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
  commentsFrom PrefixConGADT {} = Nothing
  commentsFrom RecConGADT {}    = Nothing
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
  commentsFrom PrefixConGADT {} = Nothing
  commentsFrom RecConGADT {}    = Nothing
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
  commentsBefore GRHSs {..} = priorComments grhssExt
  commentOnSameLine = const Nothing
  commentsAfter GRHSs {..} = getFollowingComments grhssExt

instance Pretty GRHSsForCase where
  pretty' (GRHSsForCase GRHSs {..}) = do
    mapM_ (pretty . fmap GRHSForCase) grhssGRHSs
    case grhssLocalBinds of
      HsValBinds {} ->
        indentedBlock $ do
          newline
          string "where " |=> pretty grhssLocalBinds
      _ -> pure ()
  commentsFrom (GRHSsForCase x) = Just $ CommentExtractable x

instance Pretty GRHSsForLambda where
  pretty' (GRHSsForLambda GRHSs {..}) = do
    mapM_ (pretty . fmap GRHSForLambda) grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedBlock $
        newlinePrefixed
          [string "where", printCommentsAnd (L epa lr) (indentedBlock . pretty)]
      _ -> return ()
  commentsFrom (GRHSsForLambda x) = Just $ CommentExtractable x

instance Pretty GRHSsForLambdaInProc where
  pretty' (GRHSsForLambdaInProc GRHSs {..}) = do
    mapM_ (pretty . fmap GRHSForLambdaInProc) grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedBlock $
        newlinePrefixed
          [string "where", printCommentsAnd (L epa lr) (indentedBlock . pretty)]
      _ -> return ()
  commentsBefore (GRHSsForLambdaInProc GRHSs {..}) = priorComments grhssExt
  commentOnSameLine = const Nothing
  commentsAfter (GRHSsForLambdaInProc GRHSs {..}) =
    getFollowingComments grhssExt

instance Pretty GRHSsForCaseInProc where
  pretty' (GRHSsForCaseInProc GRHSs {..}) = do
    mapM_ (pretty . fmap GRHSForCaseInProc) grhssGRHSs
    case grhssLocalBinds of
      HsValBinds {} ->
        indentedBlock $ do
          newline
          string "where " |=> pretty grhssLocalBinds
      _ -> pure ()
  commentsBefore (GRHSsForCaseInProc GRHSs {..}) = priorComments grhssExt
  commentOnSameLine = const Nothing
  commentsAfter (GRHSsForCaseInProc GRHSs {..}) = getFollowingComments grhssExt

instance Pretty (HsMatchContext GhcPs) where
  pretty' = prettyHsMatchContext
  commentsFrom = commentsFromHsMatchContext

prettyHsMatchContext :: HsMatchContext GhcPs -> Printer ()
prettyHsMatchContext FunRhs {..}       = pretty mc_fun
prettyHsMatchContext LambdaExpr        = return ()
prettyHsMatchContext CaseAlt           = return ()
prettyHsMatchContext IfAlt {}          = notUsedInParsedStage
prettyHsMatchContext ArrowMatchCtxt {} = notUsedInParsedStage
prettyHsMatchContext PatBindRhs {}     = notUsedInParsedStage
prettyHsMatchContext PatBindGuards {}  = notUsedInParsedStage
prettyHsMatchContext RecUpd {}         = notUsedInParsedStage
prettyHsMatchContext StmtCtxt {}       = notUsedInParsedStage
prettyHsMatchContext ThPatSplice {}    = notUsedInParsedStage
prettyHsMatchContext ThPatQuote {}     = notUsedInParsedStage
prettyHsMatchContext PatSyn {}         = notUsedInParsedStage
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsMatchContext LamCaseAlt {}     = notUsedInParsedStage
#endif
commentsFromHsMatchContext :: HsMatchContext GhcPs -> Maybe CommentExtractable
commentsFromHsMatchContext FunRhs {}         = Nothing
commentsFromHsMatchContext LambdaExpr {}     = Nothing
commentsFromHsMatchContext CaseAlt {}        = Nothing
commentsFromHsMatchContext IfAlt {}          = Nothing
commentsFromHsMatchContext ArrowMatchCtxt {} = Nothing
commentsFromHsMatchContext PatBindRhs {}     = Nothing
commentsFromHsMatchContext PatBindGuards {}  = Nothing
commentsFromHsMatchContext RecUpd {}         = Nothing
commentsFromHsMatchContext StmtCtxt {}       = Nothing
commentsFromHsMatchContext ThPatSplice {}    = Nothing
commentsFromHsMatchContext ThPatQuote {}     = Nothing
commentsFromHsMatchContext PatSyn {}         = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsMatchContext LamCaseAlt {}     = Nothing
#endif
instance Pretty (ParStmtBlock GhcPs GhcPs) where
  pretty' (ParStmtBlock _ xs _ _) = hvCommaSep $ fmap pretty xs
  commentsFrom ParStmtBlock {} = Nothing

instance Pretty ParStmtBlockInsideVerticalList where
  pretty' (ParStmtBlockInsideVerticalList (ParStmtBlock _ xs _ _)) =
    vCommaSep $ fmap pretty xs
  commentsFrom (ParStmtBlockInsideVerticalList x) = Just $ CommentExtractable x

instance Pretty RdrName where
  pretty' = pretty . PrefixOp
  commentsFrom Unqual {} = Nothing
  commentsFrom Qual {}   = Nothing
  commentsFrom Orig {}   = Nothing
  commentsFrom Exact {}  = Nothing

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' (GRHS _ [] (L _ (HsDo _ (DoExpr _) body))) = do
    string " = do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHS _ [] (L _ (HsDo _ (MDoExpr _) body))) = do
    string " = mdo"
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
  pretty' (GRHS _ guards (L _ (HsDo _ (MDoExpr _) body))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " = mdo"
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
  commentsFrom (GRHS x _ _) = Just $ CommentExtractable x

instance Pretty GRHSForCase where
  pretty' (GRHSForCase (GRHS _ [] (L _ (HsDo _ (DoExpr _) body)))) = do
    string " -> do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForCase (GRHS _ [] (L _ (HsDo _ (MDoExpr _) body)))) = do
    string " -> mdo"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForCase (GRHS _ guards (L _ (HsDo _ (DoExpr _) body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForCase (GRHS _ guards (L _ (HsDo _ (MDoExpr _) body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> mdo "
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
  commentsFrom (GRHSForCase x) = Just $ CommentExtractable x

instance Pretty GRHSForLambdaInProc where
  pretty' (GRHSForLambdaInProc (GRHS _ [] (L _ (HsCmdDo _ body)))) =
    hor <-|> ver
    where
      hor = do
        string "-> do "
        printCommentsAnd body (lined . fmap pretty)
      ver = do
        string "-> do"
        newline
        indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForLambdaInProc (GRHS _ guards (L _ (HsCmdDo _ body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForLambdaInProc (GRHS _ [] body)) = horizontal <-|> vertical
    where
      horizontal = do
        string "-> "
        pretty body
      vertical = do
        string "->"
        newline
        indentedBlock $ pretty body
  pretty' (GRHSForLambdaInProc (GRHS _ guards body)) = do
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
  commentsFrom (GRHSForLambdaInProc (GRHS x _ _)) = Just $ CommentExtractable x

instance Pretty GRHSForCaseInProc where
  pretty' (GRHSForCaseInProc (GRHS _ [] (L _ (HsCmdDo _ body)))) = do
    string " -> do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForCaseInProc (GRHS _ guards (L _ (HsCmdDo _ body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForCaseInProc (GRHS _ [] body)) = horizontal <-|> vertical
    where
      horizontal = do
        string " -> "
        pretty body
      vertical = do
        string " ->"
        newline
        indentedBlock $ pretty body
  pretty' (GRHSForCaseInProc (GRHS _ guards body)) = do
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
  commentsFrom (GRHSForCaseInProc (GRHS x _ _)) = Just $ CommentExtractable x

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
  pretty' (GRHSForLambda (GRHS _ [] (L _ (HsDo _ (MDoExpr _) body)))) =
    hor <-|> ver
    where
      hor = do
        string "-> mdo "
        printCommentsAnd body (lined . fmap pretty)
      ver = do
        string "-> mdo"
        newline
        indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForLambda (GRHS _ guards (L _ (HsDo _ (DoExpr _) body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForLambda (GRHS _ guards (L _ (HsDo _ (MDoExpr _) body)))) = do
    newline
    indentedBlock $ do
      string "| " |=> vCommaSep (fmap pretty guards)
      string " -> mdo "
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
  commentsFrom (GRHSForLambda x) = Just $ CommentExtractable x

instance Pretty GRHSForMultiwayIf where
  pretty' (GRHSForMultiwayIf (GRHS _ [] (L _ (HsDo _ (DoExpr _) body)))) = do
    string " -> do"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForMultiwayIf (GRHS _ [] (L _ (HsDo _ (MDoExpr _) body)))) = do
    string " -> mdo"
    newline
    indentedBlock $ printCommentsAnd body (lined . fmap pretty)
  pretty' (GRHSForMultiwayIf (GRHS _ guards (L _ (HsDo _ (DoExpr _) body)))) =
    indentedBlock $ do
      string "| "
      inter (comma >> newline) $ fmap pretty guards
      string " -> do "
      printCommentsAnd body (mapM_ pretty)
  pretty' (GRHSForMultiwayIf (GRHS _ guards (L _ (HsDo _ (MDoExpr _) body)))) =
    indentedBlock $ do
      string "| "
      inter (comma >> newline) $ fmap pretty guards
      string " -> mdo "
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
  commentsFrom (GRHSForMultiwayIf x) = Just $ CommentExtractable x

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
  pretty' _ =
    error
      "Documentation comments should not appear because they are treated as normal ones. EOF comment should be removed by the preprocessing."
  commentsFrom = const Nothing

instance Pretty (SpliceDecl GhcPs) where
  pretty' (SpliceDecl _ sp _) = pretty sp
  commentsFrom SpliceDecl {} = Nothing

instance Pretty (HsSplice GhcPs) where
  pretty' (HsTypedSplice _ _ _ body) = do
    string "$$"
    pretty body
  pretty' (HsUntypedSplice _ DollarSplice _ body) = do
    string "$"
    pretty body
  pretty' (HsUntypedSplice _ BareSplice _ body) = pretty body
  -- The body of a quasi-quote must not be changed by a formatter.
  -- Changing it will modify the actual behavior of the code.
  pretty' (HsQuasiQuote _ _ l _ r) =
    brackets $ do
      pretty l
      wrapWithBars $
        indentedWithFixedLevel 0 $ lined $ fmap string $ lines $ unpackFS r
  pretty' HsSpliced {} = notUsedInParsedStage
  commentsFrom (HsTypedSplice x _ _ _)   = Just $ CommentExtractable x
  commentsFrom (HsUntypedSplice x _ _ _) = Just $ CommentExtractable x
  commentsFrom HsQuasiQuote {}           = Nothing
  commentsFrom HsSpliced {}              = Nothing

instance Pretty (Pat GhcPs) where
  pretty' = prettyPat
  commentsFrom = commentsFromPat

instance Pretty PatInsidePatDecl where
  pretty' (PatInsidePatDecl (ConPat {pat_args = (InfixCon l r), ..})) =
    spaced [pretty l, pretty $ fmap InfixOp pat_con, pretty r]
  pretty' (PatInsidePatDecl x) = pretty x
  commentsFrom (PatInsidePatDecl x) = Just $ CommentExtractable x

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
prettyPat (SumPat _ x position numElem) = do
  string "(#"
  forM_ [1 .. numElem] $ \idx -> do
    if idx == position
      then string " " >> pretty x >> string " "
      else string " "
    when (idx < numElem) $ string "|"
  string "#)"
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

commentsFromPat :: Pat GhcPs -> Maybe CommentExtractable
commentsFromPat WildPat {}              = Nothing
commentsFromPat VarPat {}               = Nothing
commentsFromPat (LazyPat x _)           = Just $ CommentExtractable x
commentsFromPat (AsPat x _ _)           = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromPat (ParPat x _ _ _)        = Just $ CommentExtractable x
#else
commentsFromPat (ParPat x _)            = Just $ CommentExtractable x
#endif
commentsFromPat (BangPat x _)           = Just $ CommentExtractable x
commentsFromPat (ListPat x _)           = Just $ CommentExtractable x
commentsFromPat (TuplePat x _ _)        = Just $ CommentExtractable x
commentsFromPat (SumPat x _ _ _)        = Just $ CommentExtractable x
commentsFromPat ConPat {..}             = Just $ CommentExtractable pat_con_ext
commentsFromPat (ViewPat x _ _)         = Just $ CommentExtractable x
commentsFromPat SplicePat {}            = Nothing
commentsFromPat LitPat {}               = Nothing
commentsFromPat (NPat x _ _ _)          = Just $ CommentExtractable x
commentsFromPat (NPlusKPat x _ _ _ _ _) = Just $ CommentExtractable x
commentsFromPat (SigPat x _ _)          = Just $ CommentExtractable x

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
  commentsFrom (RecConPat x) = Just $ CommentExtractable x
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsBracket GhcPs) where
  pretty' (ExpBr _ expr) = brackets $ wrapWithBars $ pretty expr
  pretty' (PatBr _ expr) = brackets $ string "p" >> wrapWithBars (pretty expr)
  pretty' (DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' DecBrG {} = notUsedInParsedStage
  pretty' (TypBr _ expr) = brackets $ string "t" >> wrapWithBars (pretty expr)
  pretty' (VarBr _ True var) = string "'" >> pretty var
  pretty' (VarBr _ False var) = string "''" >> pretty var
  pretty' (TExpBr _ x) = typedBrackets $ pretty x
  commentsFrom ExpBr {}  = Nothing
  commentsFrom PatBr {}  = Nothing
  commentsFrom DecBrL {} = Nothing
  commentsFrom DecBrG {} = Nothing
  commentsFrom TypBr {}  = Nothing
  commentsFrom VarBr {}  = Nothing
  commentsFrom TExpBr {} = Nothing
#endif
instance Pretty SigBindFamily where
  pretty' (Sig x)        = pretty $ DeclSig x
  pretty' (Bind x)       = pretty x
  pretty' (TypeFamily x) = pretty x
  commentsFrom (Sig x)        = Just $ CommentExtractable x
  commentsFrom (Bind x)       = Just $ CommentExtractable x
  commentsFrom (TypeFamily x) = Just $ CommentExtractable x

instance Pretty EpaComment where
  pretty' EpaComment {..} = pretty ac_tok
  commentsFrom EpaComment {} = Nothing

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'Anchor'.
instance Pretty Anchor where
  pretty' _ = return ()
  commentsFrom Anchor {} = Nothing

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'SrcAnn'.
instance Pretty (SrcAnn a) where
  pretty' _ = return ()
  commentsFrom (SrcSpanAnn ep _) = Just $ CommentExtractable ep

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't pretty-print 'SrcSpan'.
instance Pretty SrcSpan where
  pretty' _ = return ()
  commentsFrom RealSrcSpan {}   = Nothing
  commentsFrom UnhelpfulSpan {} = Nothing

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
  pretty' (HsValBinds _ lr) = pretty lr
  pretty' (HsIPBinds _ x) = pretty x
  pretty' EmptyLocalBinds {} =
    error
      "This branch indicates that the bind is empty, but since calling this code means that let or where has already been output, it cannot be handled here. It should be handled higher up in the AST."
  commentsFrom (HsValBinds x _)   = Just $ CommentExtractable x
  commentsFrom (HsIPBinds x _)    = Just $ CommentExtractable x
  commentsFrom EmptyLocalBinds {} = Nothing

instance Pretty (HsValBindsLR GhcPs GhcPs) where
  pretty' (ValBinds _ methods sigs) = lined $ fmap pretty sigsAndMethods
    where
      sigsAndMethods = mkSortedLSigBindFamilyList sigs (bagToList methods) []
  pretty' XValBindsLR {} = notUsedInParsedStage
  commentsFrom ValBinds {}    = Nothing
  commentsFrom XValBindsLR {} = notUsedInParsedStage

instance Pretty (HsTupArg GhcPs) where
  pretty' (Present _ e) = pretty e
  pretty' Missing {}    = pure () -- This appears in a tuple section.
  commentsFrom (Present x _) = Just $ CommentExtractable x
  commentsFrom (Missing x)   = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty RecConField where
  pretty' (RecConField HsFieldBind {..}) = do
    pretty hfbLHS
    unless hfbPun $ do
      string " = "
      pretty hfbRHS
  commentsFrom (RecConField x) = Just $ CommentExtractable x
#else
-- | For pattern matching against a record.
instance Pretty (HsRecField' (FieldOcc GhcPs) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsRecField {..} =
    (pretty hsRecFieldLbl >> string " = ") |=> pretty hsRecFieldArg
  commentsFrom HsRecField {..} = Just $ CommentExtractable hsRecFieldAnn

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
  commentsFrom HsRecField {..} = Just $ CommentExtractable hsRecFieldAnn
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
-- | For pattern matchings against records.
instance Pretty (HsFieldBind (GenLocated (SrcAnn NoEpAnns) (FieldOcc GhcPs)) (GenLocated SrcSpanAnnA (Pat GhcPs))) where
  pretty' HsFieldBind {..} = (pretty hfbLHS >> string " = ") |=> pretty hfbRHS
  commentsFrom HsFieldBind {..} = Just $ CommentExtractable hfbAnn

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
  commentsFrom HsFieldBind {..} = Just $ CommentExtractable hfbAnn
#else
instance Pretty RecConField where
  pretty' (RecConField HsRecField {..}) = do
    pretty hsRecFieldLbl
    unless hsRecPun $ do
      string " = "
      pretty hsRecFieldArg
  commentsFrom (RecConField x) = Just $ CommentExtractable x
#endif
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty foLabel
  commentsFrom FieldOcc {} = Nothing
#else
instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty rdrNameFieldOcc
  commentsFrom FieldOcc {} = Nothing
#endif
-- HsConDeclH98Details
instance Pretty (HsConDetails Void (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs))) (GenLocated SrcSpanAnnL [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  pretty' (PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap pretty xs
      vertical = indentedBlock $ newlinePrefixed $ fmap pretty xs
  pretty' (RecCon x) =
    printCommentsAnd x $ \rec -> do
      newline
      indentedBlock $ vFields $ fmap pretty rec
  pretty' InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."
  commentsFrom PrefixCon {} = Nothing
  commentsFrom RecCon {}    = Nothing
  commentsFrom InfixCon {}  = Nothing

-- FIXME: Reconsider using a type variable.
instance Pretty a => Pretty (HsScaled GhcPs a) where
  pretty' (HsScaled _ x) = pretty x
  commentsFrom HsScaled {} = Nothing

instance Pretty (ConDeclField GhcPs) where
  pretty' ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    hCommaSep $ fmap pretty cd_fld_names
    string " :: "
    pretty cd_fld_type
  commentsFrom ConDeclField {..} = Just $ CommentExtractable cd_fld_ext

instance Pretty InfixExpr where
  pretty' (InfixExpr (L _ (HsVar _ bind))) = pretty $ fmap InfixOp bind
  pretty' (InfixExpr x)                    = pretty' x
  commentsFrom (InfixExpr x) = Just $ CommentExtractable x

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
            (HsDo _ MDoExpr {} _) -> do
              indentedWithSpace 4 (newline >> pretty (InfixExpr op)) -- 4 for "mdo "
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
          (HsDo _ (MDoExpr _) xs) -> do
            string " mdo"
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
  commentsFrom InfixApp {} = Nothing

instance Pretty a => Pretty (BooleanFormula a) where
  pretty' (Var x)    = pretty x
  pretty' (And xs)   = hvCommaSep $ fmap pretty xs
  pretty' (Or xs)    = hvBarSep $ fmap pretty xs
  pretty' (Parens x) = parens $ pretty x
  commentsFrom Var {}    = Nothing
  commentsFrom And {}    = Nothing
  commentsFrom Or {}     = Nothing
  commentsFrom Parens {} = Nothing

instance Pretty (FieldLabelStrings GhcPs) where
  pretty' (FieldLabelStrings xs) = hDotSep $ fmap pretty xs
  commentsFrom FieldLabelStrings {} = Nothing

instance Pretty (AmbiguousFieldOcc GhcPs) where
  pretty' (Unambiguous _ name) = pretty name
  pretty' (Ambiguous _ name)   = pretty name
  commentsFrom Unambiguous {} = Nothing
  commentsFrom Ambiguous {}   = Nothing

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
  commentsFrom ImportDecl {..} = Just $ CommentExtractable ideclExt

packageName :: ImportDecl GhcPs -> Maybe StringLiteral
#if MIN_VERSION_ghc_lib_parser(9,4,1)
packageName (ideclPkgQual -> RawPkgQual name) = Just name
packageName _ = Nothing
#else
packageName = ideclPkgQual
#endif
instance Pretty (HsDerivingClause GhcPs) where
  pretty' HsDerivingClause { deriv_clause_strategy = Just strategy@(L _ ViaStrategy {})
                           , ..
                           } =
    spaced [string "deriving", pretty deriv_clause_tys, pretty strategy]
  pretty' HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      pretty x
      space
    pretty deriv_clause_tys
  commentsFrom HsDerivingClause {..} =
    Just $ CommentExtractable deriv_clause_ext

instance Pretty (DerivClauseTys GhcPs) where
  pretty' (DctSingle _ ty) = parens $ pretty ty
  pretty' (DctMulti _ ts)  = hvTuple $ fmap pretty ts
  commentsFrom DctSingle {} = Nothing
  commentsFrom DctMulti {}  = Nothing

instance Pretty OverlapMode where
  pretty' NoOverlap {}    = notUsedInParsedStage
  pretty' Overlappable {} = string "{-# OVERLAPPABLE #-}"
  pretty' Overlapping {}  = string "{-# OVERLAPPING #-}"
  pretty' Overlaps {}     = string "{-# OVERLAPS #-}"
  pretty' Incoherent {}   = string "{-# INCOHERENT #-}"
  commentsFrom NoOverlap {}    = Nothing
  commentsFrom Overlappable {} = Nothing
  commentsFrom Overlapping {}  = Nothing
  commentsFrom Overlaps {}     = Nothing
  commentsFrom Incoherent {}   = Nothing

instance Pretty StringLiteral where
  pretty' = output
  commentsFrom StringLiteral {} = Nothing

-- | This instance is for type family declarations inside a class declaration.
instance Pretty (FamilyDecl GhcPs) where
  pretty' FamilyDecl {..} = do
    string $
      case fdInfo of
        DataFamily          -> "data"
        OpenTypeFamily      -> "type"
        ClosedTypeFamily {} -> "type"
    case fdTopLevel of
      TopLevel    -> string " family "
      NotTopLevel -> space
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
  commentsFrom FamilyDecl {..} = Just $ CommentExtractable fdExt

instance Pretty (FamilyResultSig GhcPs) where
  pretty' NoSig {} = pure ()
  pretty' (KindSig _ x) = do
    string ":: "
    pretty x
  pretty' (TyVarSig _ x) = pretty x
  commentsFrom NoSig {}    = Nothing
  commentsFrom KindSig {}  = Nothing
  commentsFrom TyVarSig {} = Nothing

instance Pretty (HsTyVarBndr a GhcPs) where
  pretty' (UserTyVar _ _ x) = pretty x
  pretty' (KindedTyVar _ _ name ty) =
    parens $ do
      pretty name
      string " :: "
      pretty ty
  commentsFrom (UserTyVar x _ _)     = Just $ CommentExtractable x
  commentsFrom (KindedTyVar x _ _ _) = Just $ CommentExtractable x

instance Pretty (InjectivityAnn GhcPs) where
  pretty' (InjectivityAnn _ from to) = do
    pretty from
    string " -> "
    spaced $ fmap pretty to
  commentsFrom (InjectivityAnn x _ _) = Just $ CommentExtractable x

instance Pretty (ArithSeqInfo GhcPs) where
  pretty' (From from) = brackets $ spaced [pretty from, string ".."]
  pretty' (FromThen from next) =
    brackets $ spaced [pretty from >> comma >> pretty next, string ".."]
  pretty' (FromTo from to) =
    brackets $ spaced [pretty from, string "..", pretty to]
  pretty' (FromThenTo from next to) =
    brackets $
    spaced [pretty from >> comma >> pretty next, string "..", pretty to]
  commentsFrom From {}       = Nothing
  commentsFrom FromThen {}   = Nothing
  commentsFrom FromTo {}     = Nothing
  commentsFrom FromThenTo {} = Nothing

instance Pretty (HsForAllTelescope GhcPs) where
  pretty' HsForAllVis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_vis_bndrs
    dot
  pretty' HsForAllInvis {..} = do
    string "forall "
    spaced $ fmap pretty hsf_invis_bndrs
    dot
  commentsFrom HsForAllVis {..}   = Just $ CommentExtractable hsf_xvis
  commentsFrom HsForAllInvis {..} = Just $ CommentExtractable hsf_xinvis

instance Pretty InfixOp where
  pretty' (InfixOp (Unqual name)) = backticksIfNotSymbol name $ pretty name
  pretty' (InfixOp (Qual modName name)) =
    backticksIfNotSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (InfixOp Orig {}) = notUsedInParsedStage
  pretty' (InfixOp (Exact name)) = backticksIfNotSymbol occ $ pretty occ
    where
      occ = occName name
  commentsFrom (InfixOp x) = Just $ CommentExtractable x

instance Pretty PrefixOp where
  pretty' (PrefixOp (Unqual name)) = parensIfSymbol name $ pretty name
  pretty' (PrefixOp (Qual modName name)) =
    parensIfSymbol name $ do
      pretty modName
      string "."
      pretty name
  pretty' (PrefixOp Orig {}) = notUsedInParsedStage
  pretty' (PrefixOp (Exact name)) = parensIfSymbol occ $ pretty occ
    where
      occ = occName name
  commentsFrom (PrefixOp x) = Just $ CommentExtractable x

instance Pretty Context where
  pretty' (Context xs) =
    pretty (HorizontalContext xs) <-|> pretty (VerticalContext xs)
  commentsFrom Context {} = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty HorizontalContext where
  pretty' (HorizontalContext xs) =
    constraintsParens $ printCommentsAnd xs (hCommaSep . fmap pretty)
    where
      constraintsParens =
        case xs of
          (L _ [])  -> parens
          (L _ [_]) -> id
          _         -> parens
  commentsFrom HorizontalContext {} = Nothing

instance Pretty VerticalContext where
  pretty' (VerticalContext full@(L _ [])) =
    printCommentsAnd full (const $ string "()")
  pretty' (VerticalContext full@(L _ [x])) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext xs) = printCommentsAnd xs (vTuple . fmap pretty)
  commentsFrom VerticalContext {} = Nothing
#else
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
  commentsFrom HorizontalContext {} = Nothing

instance Pretty VerticalContext where
  pretty' (VerticalContext Nothing) = pure ()
  pretty' (VerticalContext (Just (L _ []))) = string "()"
  pretty' (VerticalContext (Just full@(L _ [x]))) =
    printCommentsAnd full (const $ pretty x)
  pretty' (VerticalContext (Just xs)) =
    printCommentsAnd xs (vTuple . fmap pretty)
  commentsFrom VerticalContext {} = Nothing
#endif
-- Wrap a value of this type with 'ModulenameWithPrefix' to print it with
-- the "module " prefix.
instance Pretty ModuleName where
  pretty' = output
  commentsFrom = const Nothing

instance Pretty ModuleNameWithPrefix where
  pretty' (ModuleNameWithPrefix name) = spaced [string "module", pretty name]
  commentsFrom ModuleNameWithPrefix {} = Nothing

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
  pretty' IEGroup {} =
    error "Never appears as doc comments are treated as normal ones."
  pretty' IEDoc {} =
    error "Never appears as doc comments are treated as normal ones."
  pretty' IEDocNamed {} =
    error "Never appears as doc comments are treated as normal ones."
  commentsFrom IEVar {}               = Nothing
  commentsFrom (IEThingAbs x _)       = Just $ CommentExtractable x
  commentsFrom (IEThingAll x _)       = Just $ CommentExtractable x
  commentsFrom (IEThingWith x _ _ _)  = Just $ CommentExtractable x
  commentsFrom (IEModuleContents x _) = Just $ CommentExtractable x
  commentsFrom IEGroup {}             = Nothing
  commentsFrom IEDoc {}               = Nothing
  commentsFrom IEDocNamed {}          = Nothing

instance Pretty (FamEqn GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' FamEqn {..} = do
    pretty feqn_tycon
    spacePrefixed $ fmap pretty feqn_pats
    string " = "
    pretty feqn_rhs
  commentsFrom FamEqn {..} = Just $ CommentExtractable feqn_ext

-- | Pretty-print a data instance.
instance Pretty (FamEqn GhcPs (HsDataDefn GhcPs)) where
  pretty' FamEqn {..} = do
    spaced $ string "data instance" : pretty feqn_tycon : fmap pretty feqn_pats
    pretty feqn_rhs
  commentsFrom FamEqn {..} = Just $ CommentExtractable feqn_ext

-- | HsArg (LHsType GhcPs) (LHsType GhcPs)
instance Pretty (HsArg (GenLocated SrcSpanAnnA (HsType GhcPs)) (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' (HsValArg x)    = pretty x
  pretty' (HsTypeArg _ x) = string "@" >> pretty x
  pretty' HsArgPar {}     = notUsedInParsedStage
  commentsFrom HsValArg {}  = Nothing
  commentsFrom HsTypeArg {} = Nothing
  commentsFrom HsArgPar {}  = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (HsQuote GhcPs) where
  pretty' (ExpBr _ x) = brackets $ wrapWithBars $ pretty x
  pretty' (PatBr _ x) = brackets $ string "p" >> wrapWithBars (pretty x)
  pretty' (DecBrL _ decls) =
    brackets $ string "d| " |=> lined (fmap pretty decls) >> string " |"
  pretty' DecBrG {} = notUsedInParsedStage
  pretty' (TypBr _ x) = brackets $ string "t" >> wrapWithBars (pretty x)
  pretty' (VarBr _ True x) = string "'" >> pretty x
  pretty' (VarBr _ False x) = string "''" >> pretty x
  commentsFrom ExpBr {}  = Nothing
  commentsFrom PatBr {}  = Nothing
  commentsFrom DecBrL {} = Nothing
  commentsFrom DecBrG {} = Nothing
  commentsFrom TypBr {}  = Nothing
  commentsFrom VarBr {}  = Nothing
#endif
instance Pretty (WarnDecls GhcPs) where
  pretty' (Warnings _ _ x) = lined $ fmap pretty x
  commentsFrom Warnings {..} = Just $ CommentExtractable wd_ext

instance Pretty (WarnDecl GhcPs) where
  pretty' (Warning _ names deprecatedOrWarning) =
    case deprecatedOrWarning of
      DeprecatedTxt _ reasons -> prettyWithTitleReasons "DEPRECATED" reasons
      WarningTxt _ reasons    -> prettyWithTitleReasons "WARNING" reasons
    where
      prettyWithTitleReasons title reasons =
        lined
          [ string $ "{-# " ++ title
          , spaced
              [hCommaSep $ fmap pretty names, hCommaSep $ fmap pretty reasons]
          , string " #-}"
          ]
  commentsFrom (Warning x _ _) = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (WithHsDocIdentifiers StringLiteral GhcPs) where
  pretty' WithHsDocIdentifiers {..} = pretty hsDocString
  commentsFrom WithHsDocIdentifiers {} = Nothing
#endif
-- | 'Pretty' for 'LIEWrappedName (IdP GhcPs)'
instance Pretty (IEWrappedName RdrName) where
  pretty' (IEName name)      = pretty name
  pretty' (IEPattern _ name) = spaced [string "pattern", pretty name]
  pretty' (IEType _ name)    = string "type " >> pretty name
  commentsFrom IEName {}    = Nothing
  commentsFrom IEPattern {} = Nothing
  commentsFrom IEType {}    = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
instance Pretty (DotFieldOcc GhcPs) where
  pretty' DotFieldOcc {..} = printCommentsAnd dfoLabel (string . unpackFS)
  commentsFrom DotFieldOcc {..} = Just $ CommentExtractable dfoExt
#else
instance Pretty (HsFieldLabel GhcPs) where
  pretty' HsFieldLabel {..} = printCommentsAnd hflLabel (string . unpackFS)
  commentsFrom HsFieldLabel {..} = Just $ CommentExtractable hflExt
#endif
instance Pretty (RuleDecls GhcPs) where
  pretty' HsRules {..} =
    lined $ string "{-# RULES" : fmap pretty rds_rules ++ [string " #-}"]
  commentsFrom HsRules {..} = Just $ CommentExtractable rds_ext

instance Pretty (RuleDecl GhcPs) where
  pretty' HsRule {..} =
    spaced
      [ printCommentsAnd rd_name (doubleQuotes . string . unpackFS . snd)
      , pretty rd_lhs
      , string "="
      , pretty rd_rhs
      ]
  commentsFrom HsRule {..} = Just $ CommentExtractable rd_ext

instance Pretty OccName where
  pretty' = output
  commentsFrom = const Nothing

instance Pretty (DerivDecl GhcPs)
  -- TODO: Handle deriving strategies.
                                       where
  pretty' DerivDecl {..} = do
    string "deriving instance "
    pretty deriv_type
  commentsFrom DerivDecl {..} = Just $ CommentExtractable deriv_ext

-- | 'Pretty' for 'LHsSigWcType GhcPs'.
instance Pretty (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsSigType GhcPs))) where
  pretty' HsWC {..} = pretty hswc_body
  commentsFrom HsWC {} = Nothing

-- | 'Pretty' for 'LHsWcType'
instance Pretty (HsWildCardBndrs GhcPs (GenLocated SrcSpanAnnA (HsType GhcPs))) where
  pretty' HsWC {..} = pretty hswc_body
  commentsFrom HsWC {} = Nothing

instance Pretty (StandaloneKindSig GhcPs) where
  pretty' (StandaloneKindSig _ name kind) =
    spaced [string "type", pretty name, string "::", pretty kind]
  commentsFrom (StandaloneKindSig x _ _) = Just $ CommentExtractable x

instance Pretty (DefaultDecl GhcPs) where
  pretty' (DefaultDecl _ xs) =
    spaced [string "default", hTuple $ fmap pretty xs]
  commentsFrom (DefaultDecl x _) = Just $ CommentExtractable x

instance Pretty (ForeignDecl GhcPs)
  -- TODO: Implement correctly.
                                where
  pretty' ForeignImport {fd_fi = (CImport _ safety _ _ _)} = do
    string "foreign import ccall "
    pretty safety
    string " \"test\" test :: IO ()"
  pretty' ForeignExport {} =
    string "foreign export ccall \"test\" test :: IO ()"
  commentsFrom ForeignImport {..} = Just $ CommentExtractable fd_i_ext
  commentsFrom ForeignExport {..} = Just $ CommentExtractable fd_e_ext

instance Pretty Safety where
  pretty' PlaySafe          = string "safe"
  pretty' PlayInterruptible = string "interruptible"
  pretty' PlayRisky         = string "unsafe"
  commentsFrom PlaySafe          = Nothing
  commentsFrom PlayInterruptible = Nothing
  commentsFrom PlayRisky         = Nothing

instance Pretty (AnnDecl GhcPs) where
  pretty' (HsAnnotation _ _ (ValueAnnProvenance name) expr) =
    spaced [string "{-# ANN", pretty name, pretty expr, string "#-}"]
  pretty' (HsAnnotation _ _ (TypeAnnProvenance name) expr) =
    spaced [string "{-# ANN type", pretty name, pretty expr, string "#-}"]
  pretty' (HsAnnotation _ _ ModuleAnnProvenance expr) =
    spaced [string "{-# ANN module", pretty expr, string "#-}"]
  commentsFrom (HsAnnotation x _ _ _) = Just $ CommentExtractable x

instance Pretty (RoleAnnotDecl GhcPs) where
  pretty' (RoleAnnotDecl _ name roles) =
    spaced $
    [string "type role", pretty name] ++
    fmap (maybe (string "_") pretty . unLoc) roles
  commentsFrom (RoleAnnotDecl x _ _) = Just $ CommentExtractable x

instance Pretty Role where
  pretty' Nominal          = string "nominal"
  pretty' Representational = string "representational"
  pretty' Phantom          = string "phantom"
  commentsFrom Nominal          = Nothing
  commentsFrom Representational = Nothing
  commentsFrom Phantom          = Nothing

instance Pretty (TyFamInstDecl GhcPs) where
  pretty' TyFamInstDecl {..} = do
    string "type instance "
    pretty tfid_eqn
  commentsFrom TyFamInstDecl {..} = Just $ CommentExtractable tfid_xtn

instance Pretty (DataFamInstDecl GhcPs) where
  pretty' DataFamInstDecl {..} = pretty dfid_eqn
  commentsFrom DataFamInstDecl {} = Nothing

instance Pretty (PatSynBind GhcPs GhcPs) where
  pretty' PSB {..} = do
    string "pattern "
    case psb_args of
      InfixCon l r -> spaced [pretty l, pretty $ fmap InfixOp psb_id, pretty r]
      _            -> spaced [pretty psb_id, pretty psb_args]
    spacePrefixed [pretty psb_dir, pretty $ fmap PatInsidePatDecl psb_def]
    case psb_dir of
      ExplicitBidirectional matches -> do
        newline
        indentedBlock $ string "where " |=> pretty matches
      _ -> pure ()
  commentsFrom PSB {..} = Just $ CommentExtractable psb_ext

-- | 'Pretty' for 'HsPatSynDetails'.
instance Pretty (HsConDetails Void (GenLocated SrcSpanAnnN RdrName) [RecordPatSynField GhcPs]) where
  pretty' (PrefixCon _ xs) = spaced $ fmap pretty xs
  pretty' (RecCon rec) = hFields $ fmap pretty rec
  pretty' InfixCon {} =
    error
      "Cannot handle here because `InfixCon` does not have the information of the constructor."
  commentsFrom PrefixCon {} = Nothing
  commentsFrom RecCon {}    = Nothing
  commentsFrom InfixCon {}  = Nothing

instance Pretty (FixitySig GhcPs) where
  pretty' (FixitySig _ names fixity) =
    spaced [pretty fixity, hCommaSep $ fmap (pretty . fmap InfixOp) names]
  commentsFrom FixitySig {} = Nothing

instance Pretty Fixity where
  pretty' (Fixity _ level dir) = spaced [pretty dir, string $ show level]
  commentsFrom Fixity {} = Nothing

instance Pretty FixityDirection where
  pretty' InfixL = string "infixl"
  pretty' InfixR = string "infixr"
  pretty' InfixN = string "infix"
  commentsFrom InfixL {} = Nothing
  commentsFrom InfixR {} = Nothing
  commentsFrom InfixN {} = Nothing

instance Pretty InlinePragma where
  pretty' InlinePragma {..} = pretty inl_inline
  commentsFrom InlinePragma {} = Nothing

instance Pretty InlineSpec where
  pretty' = prettyInlineSpec
  commentsFrom = commentsFromInlineSpec

prettyInlineSpec :: InlineSpec -> Printer ()
prettyInlineSpec Inline {} = string "INLINE"
prettyInlineSpec Inlinable {} = string "INLINABLE"
prettyInlineSpec NoInline {} = string "NOINLINE"
prettyInlineSpec NoUserInlinePrag =
  error
    "This branch is executed if the inline pragma is not written, but executing this branch means that the pragma is already about to be output, so something goes wrong."
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyInlineSpec Opaque {} = string "OPAQUE"
#endif
commentsFromInlineSpec :: InlineSpec -> Maybe CommentExtractable
commentsFromInlineSpec Inline {}           = Nothing
commentsFromInlineSpec Inlinable {}        = Nothing
commentsFromInlineSpec NoInline {}         = Nothing
commentsFromInlineSpec NoUserInlinePrag {} = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromInlineSpec Opaque {}           = Nothing
#endif
instance Pretty (HsPatSynDir GhcPs) where
  pretty' Unidirectional           = string "<-"
  pretty' ImplicitBidirectional    = string "="
  pretty' ExplicitBidirectional {} = string "<-"
  commentsFrom Unidirectional           = Nothing
  commentsFrom ImplicitBidirectional    = Nothing
  commentsFrom ExplicitBidirectional {} = Nothing

instance Pretty (HsOverLit GhcPs) where
  pretty' OverLit {..} = pretty ol_val
  commentsFrom OverLit {} = Nothing

instance Pretty OverLitVal where
  pretty' (HsIntegral x)   = pretty x
  pretty' (HsFractional x) = pretty x
  pretty' (HsIsString _ x) = string $ unpackFS x
  commentsFrom HsIntegral {}   = Nothing
  commentsFrom HsFractional {} = Nothing
  commentsFrom HsIsString {}   = Nothing

instance Pretty IntegralLit where
  pretty' IL {..} = string $ show il_value
  commentsFrom IL {} = Nothing

instance Pretty FractionalLit where
  pretty' = output
  commentsFrom FL {} = Nothing

instance Pretty (HsLit GhcPs) where
  pretty' x@(HsChar _ _) = output x
  pretty' x@HsCharPrim {} = output x
  pretty' HsInt {} = notUsedInParsedStage
  pretty' (HsIntPrim _ x) = string $ show x ++ "#"
  pretty' HsWordPrim {} = notUsedInParsedStage
  pretty' HsInt64Prim {} = notUsedInParsedStage
  pretty' HsWord64Prim {} = notUsedInParsedStage
  pretty' HsInteger {} = notUsedInParsedStage
  pretty' HsRat {} = notUsedInParsedStage
  pretty' (HsFloatPrim _ x) = pretty x >> string "#"
  pretty' HsDoublePrim {} = notUsedInParsedStage
  pretty' x =
    case x of
      HsString {}     -> prettyString
      HsStringPrim {} -> prettyString
    where
      prettyString =
        case lines $ showOutputable x of
          [] -> pure ()
          [l] -> string l
          (s:ss) ->
            string "" |=> do
              string s
              newline
              indentedWithSpace (-1) $
                lined $ fmap (string . dropWhile (/= '\\')) ss
  commentsFrom HsChar {}       = Nothing
  commentsFrom HsCharPrim {}   = Nothing
  commentsFrom HsString {}     = Nothing
  commentsFrom HsStringPrim {} = Nothing
  commentsFrom HsInt {}        = Nothing
  commentsFrom HsIntPrim {}    = Nothing
  commentsFrom HsWordPrim {}   = Nothing
  commentsFrom HsInt64Prim {}  = Nothing
  commentsFrom HsWord64Prim {} = Nothing
  commentsFrom HsInteger {}    = Nothing
  commentsFrom HsRat {}        = Nothing
  commentsFrom HsFloatPrim {}  = Nothing
  commentsFrom HsDoublePrim {} = Nothing

instance Pretty (HsPragE GhcPs) where
  pretty' (HsPragSCC _ _ x) = spaced [string "{-# SCC", pretty x, string "#-}"]
  commentsFrom (HsPragSCC x _ _) = Just $ CommentExtractable x

instance Pretty HsIPName where
  pretty' (HsIPName x) = string $ unpackFS x
  commentsFrom HsIPName {} = Nothing

instance Pretty HsTyLit where
  pretty' (HsNumTy _ x)  = string $ show x
  pretty' (HsStrTy _ x)  = string $ show x
  pretty' (HsCharTy _ x) = string $ show x
  commentsFrom HsNumTy {}  = Nothing
  commentsFrom HsStrTy {}  = Nothing
  commentsFrom HsCharTy {} = Nothing

instance Pretty (HsPatSigType GhcPs) where
  pretty' HsPS {..} = pretty hsps_body
  commentsFrom HsPS {..} = Just $ CommentExtractable hsps_ext

instance Pretty (HsIPBinds GhcPs) where
  pretty' (IPBinds _ xs) = lined $ fmap pretty xs
  commentsFrom IPBinds {} = Nothing

instance Pretty (IPBind GhcPs) where
  pretty' = prettyIPBind
  commentsFrom (IPBind x _ _) = Just $ CommentExtractable x

prettyIPBind :: IPBind GhcPs -> Printer ()
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyIPBind (IPBind _ l r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#else
prettyIPBind (IPBind _ (Right _) _) = notUsedInParsedStage
prettyIPBind (IPBind _ (Left l) r) =
  spaced [string "?" >> pretty l, string "=", pretty r]
#endif
-- TODO: 'instance Pretty XViaStrategyPs'
instance Pretty (DerivStrategy GhcPs) where
  pretty' StockStrategy {}                    = string "stock"
  pretty' AnyclassStrategy {}                 = string "anyclass"
  pretty' NewtypeStrategy {}                  = string "newtype"
  pretty' (ViaStrategy (XViaStrategyPs _ ty)) = string "via " >> pretty ty
  commentsFrom (StockStrategy x)                  = Just $ CommentExtractable x
  commentsFrom (AnyclassStrategy x)               = Just $ CommentExtractable x
  commentsFrom (NewtypeStrategy x)                = Just $ CommentExtractable x
  commentsFrom (ViaStrategy (XViaStrategyPs x _)) = Just $ CommentExtractable x

instance Pretty (RecordPatSynField GhcPs) where
  pretty' RecordPatSynField {..} = pretty recordPatSynField
  commentsFrom RecordPatSynField {} = Nothing

instance Pretty (HsCmdTop GhcPs) where
  pretty' (HsCmdTop _ cmd) = pretty cmd
  commentsFrom HsCmdTop {} = Nothing

instance Pretty (HsCmd GhcPs) where
  pretty' = prettyHsCmd
  commentsFrom = commentsFromHsCmd

prettyHsCmd :: HsCmd GhcPs -> Printer ()
prettyHsCmd (HsCmdArrApp _ f arg HsHigherOrderApp True) =
  spaced [pretty f, string "-<<", pretty arg]
prettyHsCmd (HsCmdArrApp _ f arg HsHigherOrderApp False) =
  spaced [pretty arg, string ">>-", pretty f]
prettyHsCmd (HsCmdArrApp _ f arg HsFirstOrderApp True) =
  spaced [pretty f, string "-<", pretty arg]
prettyHsCmd (HsCmdArrApp _ f arg HsFirstOrderApp False) =
  spaced [pretty arg, string ">-", pretty f]
prettyHsCmd (HsCmdArrForm _ f _ _ args) =
  bananaBrackets $ spaced $ pretty f : fmap pretty args
prettyHsCmd (HsCmdApp _ f arg) = spaced [pretty f, pretty arg]
prettyHsCmd (HsCmdLam _ x) = pretty $ MatchGroupForLambdaInProc x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdPar _ _ x _) = parens $ pretty x
#else
prettyHsCmd (HsCmdPar _ x) = parens $ pretty x
#endif
prettyHsCmd (HsCmdCase _ cond arms) = do
  spaced [string "case", pretty cond, string "of"]
  newline
  indentedBlock $ pretty $ MatchGroupForCaseInProc arms
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdLamCase _ _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty $ MatchGroupForCaseInProc arms
#else
prettyHsCmd (HsCmdLamCase _ arms) = do
  string "\\case"
  newline
  indentedBlock $ pretty $ MatchGroupForCaseInProc arms
#endif
prettyHsCmd (HsCmdIf _ _ cond t f) = do
  string "if "
  pretty cond
  newline
  indentedBlock $ lined [string "then " >> pretty t, string "else " >> pretty f]
#if MIN_VERSION_ghc_lib_parser(9,4,1)
prettyHsCmd (HsCmdLet _ _ binds _ expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#else
prettyHsCmd (HsCmdLet _ binds expr) =
  lined [string "let " |=> pretty binds, string " in " |=> pretty expr]
#endif
prettyHsCmd (HsCmdDo _ stmts) = do
  string "do"
  newline
  indentedBlock $ printCommentsAnd stmts (lined . fmap pretty)

commentsFromHsCmd :: HsCmd GhcPs -> Maybe CommentExtractable
commentsFromHsCmd (HsCmdArrApp x _ _ _ _)  = Just $ CommentExtractable x
commentsFromHsCmd (HsCmdArrForm x _ _ _ _) = Just $ CommentExtractable x
commentsFromHsCmd (HsCmdApp x _ _)         = Just $ CommentExtractable x
commentsFromHsCmd HsCmdLam {}              = Nothing
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsCmd (HsCmdPar x _ _ _)       = Just $ CommentExtractable x
#else
commentsFromHsCmd (HsCmdPar x _)           = Just $ CommentExtractable x
#endif
commentsFromHsCmd (HsCmdCase x _ _)        = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsCmd (HsCmdLamCase x _ _)     = Just $ CommentExtractable x
#else
commentsFromHsCmd (HsCmdLamCase x _)       = Just $ CommentExtractable x
#endif
commentsFromHsCmd (HsCmdIf x _ _ _ _)      = Just $ CommentExtractable x
#if MIN_VERSION_ghc_lib_parser(9,4,1)
commentsFromHsCmd (HsCmdLet x _ _ _ _)     = Just $ CommentExtractable x
#else
commentsFromHsCmd (HsCmdLet x _ _)         = Just $ CommentExtractable x
#endif
commentsFromHsCmd (HsCmdDo x _)            = Just $ CommentExtractable x

instance Pretty ListComprehension where
  pretty' ListComprehension {..} = horizontal <-|> vertical
    where
      horizontal =
        brackets $
        spaced
          [pretty listCompLhs, string "|", hCommaSep $ fmap pretty listCompRhs]
      vertical = do
        string "[ "
        pretty $ fmap StmtLRInsideVerticalList listCompLhs
        newline
        forM_ (stmtsAndPrefixes listCompRhs) $ \(p, x) -> do
          string p |=> pretty (fmap StmtLRInsideVerticalList x)
          newline
        string "]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)
  commentsFrom ListComprehension {} = Nothing

instance Pretty DoExpression where
  pretty' DoExpression {..} =
    (string pref >> space) |=> lined (fmap pretty doStmts)
    where
      pref =
        case doOrMdo of
          Do  -> "do"
          Mdo -> "mdo"
  commentsFrom DoExpression {} = Nothing

instance Pretty LetIn where
  pretty' LetIn {..} =
    lined [string "let " |=> pretty letBinds, string " in " |=> pretty inExpr]
  commentsFrom LetIn {} = Nothing

-- | Marks an AST node as never appearing in the AST.
--
-- Some AST node types are only used in the renaming or type-checking phase.
notUsedInParsedStage :: HasCallStack => a
notUsedInParsedStage =
  error
    "This AST should never appears in an AST. It only appears in the renaming or type checked stages."
#if !MIN_VERSION_ghc_lib_parser(9,4,1)
-- | Marks an AST node as it is used only for Haskell Program Coverage.
forHpc :: HasCallStack => a
forHpc = error "This AST type is for the use of Haskell Program Coverage."
#endif
