{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Void
import           Debug.Trace
import           Generics.SYB                      hiding (Infix, Prefix)
import           GHC.Data.Bag
import           GHC.Data.BooleanFormula
import           GHC.Hs
import           GHC.Hs.Dump
import           GHC.Types.Fixity
import           GHC.Types.Name.Reader
import           GHC.Types.SrcLoc
import           GHC.Unit
import           HIndent.Applicative
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.Indent
import           HIndent.Pretty.Combinators.Inside
import           HIndent.Pretty.Combinators.Op
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Pretty.Imports.Sort
import           HIndent.Pretty.ModuleDeclaration
import           HIndent.Pretty.Pragma
import           HIndent.Types

data SigOrMethods
  = Sig (LSig GhcPs)
  | Method (LHsBindLR GhcPs GhcPs)

newtype InfixExpr =
  InfixExpr (LHsExpr GhcPs)

data InfixApp =
  InfixApp
    { lhs                :: LHsExpr GhcPs
    , op                 :: LHsExpr GhcPs
    , rhs                :: LHsExpr GhcPs
    , immediatelyAfterDo :: Bool
    }

-- | Pretty print including comments.
--
-- TODO: Define `pretty` as a top-level function. It should have only one
-- definition, and it must not be changed in an instance declaration.
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
  pretty :: a -> Printer ()
  pretty p = do
    printCommentsBefore p
    pretty' p
    printCommentsSameLine p
    printCommentsAfter p
  pretty' :: a -> Printer ()
  printCommentsBefore :: a -> Printer ()
  printCommentsBefore p =
    forM_ (commentsBefore p) $ \x -> do
      pretty x
      newline
  printCommentsSameLine :: a -> Printer ()
  printCommentsSameLine (commentsSameLine -> Just (L sp c)) = do
    col <- gets psColumn
    if col == 0
      then indentedWithLevel (fromIntegral $ srcSpanStartCol $ anchor sp) $
           pretty c
      else do
        space
        pretty c
    eolCommentsArePrinted
  printCommentsSameLine _ = return ()
  printCommentsAfter :: a -> Printer ()
  printCommentsAfter p =
    case commentsAfter p of
      [] -> return ()
      xs -> do
        isThereCommentsOnSameLine <- gets psEolComment
        unless isThereCommentsOnSameLine newline
        forM_ xs $ \(L loc c) -> do
          let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
          indentedWithLevel col $ pretty c
          eolCommentsArePrinted
  -- These functions must return comments that only this node can fetch. In
  -- other words, these functions must not return comments that child nodes
  -- can fetch.
  commentsBefore :: a -> [LEpaComment]
  commentsBefore = const []
  commentsSameLine :: a -> Maybe LEpaComment
  commentsSameLine = const Nothing
  commentsAfter :: a -> [LEpaComment]
  commentsAfter = const []

instance Pretty HsModule where
  pretty' m = inter blankline printers
    -- TODO: Refactor this 'where' clause.
    where
      printers = snd <$> filter fst pairs
      pairs =
        [ (pragmaExists m, outputPragmas m)
        , (moduleDeclarationExists m, outputModuleDeclaration m)
        , (importsExist m, outputImports m)
        , (declsExist m, outputDecls)
        ]
      outputDecls =
        mapM_ (\(x, sp) -> pretty x >> fromMaybe (return ()) sp) $
        addSeparator $ hsmodDecls m
      addSeparator []     = []
      addSeparator [x]    = [(x, Nothing)]
      addSeparator (x:xs) = (x, Just $ separator $ unLoc x) : addSeparator xs
      separator SigD {} = newline
      separator _       = blankline
      declsExist = not . null . hsmodDecls
      outputImports =
        inter blankline .
        fmap (outputImportGroup . sortImportsByName) .
        groupImports . sortImportsByLocation . hsmodImports
      outputImportGroup = inter newline . fmap pretty
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
  commentsSameLine (L l _) = commentsSameLine l
  commentsAfter (L l _) = commentsAfter l

instance Pretty (HsDecl GhcPs) where
  pretty' (TyClD _ d)    = pretty d
  pretty' (InstD _ inst) = pretty inst
  pretty' DerivD {}      = undefined
  pretty' (ValD _ bind)  = pretty bind
  pretty' (SigD _ s)     = insideSignature $ pretty s
  pretty' KindSigD {}    = undefined
  pretty' DefD {}        = undefined
  pretty' ForD {}        = undefined
  pretty' WarningD {}    = undefined
  pretty' AnnD {}        = undefined
  pretty' RuleD {}       = undefined
  pretty' (SpliceD _ sp) = pretty sp
  pretty' DocD {}        = return ()
  pretty' RoleAnnotD {}  = undefined

instance Pretty (TyClDecl GhcPs) where
  pretty' SynDecl {..} = do
    string "type "
    -- TODO: Merge this case with the one in 'ClassDecl's branch.
    case tcdFixity of
      Prefix ->
        spaced $ (pretty tcdLName) : fmap output (hsq_explicit tcdTyVars)
      Infix ->
        case hsq_explicit tcdTyVars of
          (l:r:xs)
            -- TODO: Handle comments around 'tcdLName'.
           -> do
            spaced [output l, infixOp $ unLoc tcdLName, output r]
            forM_ xs $ \x -> do
              space
              output x
          _ -> error "Not enough parameters are given."
    hor `ifFitsOnOneLineOrElse` ver
    where
      hor = do
        string " = "
        pretty tcdRhs
      ver =
        indentedWithSpace 3 $ do
          newline
          string "= "
          indentedBlock $ pretty tcdRhs
  pretty' DataDecl {..} = do
    case dd_ND tcdDataDefn of
      DataType -> string "data "
      NewType  -> string "newtype "
    output tcdLName
    forM_ (hsq_explicit tcdTyVars) $ \x -> do
      space
      output x
    pretty tcdDataDefn
  pretty' ClassDecl {..} = do
    horHead `ifFitsOnOneLineOrElse` verHead
    newline
    indentedBlock $ inter newline $ fmap pretty sigsAndMethods
    where
      horHead = do
        string "class "
        case tcdFixity of
          Prefix ->
            spaced $ (pretty tcdLName) : fmap output (hsq_explicit tcdTyVars)
          Infix ->
            case hsq_explicit tcdTyVars of
              (l:r:xs)
                -- TODO: Handle comments around 'tcdLName'.
               -> do
                parens $ spaced [output l, infixOp $ unLoc tcdLName, output r]
                forM_ xs $ \x -> do
                  space
                  output x
              _ -> error "Not enough parameters are given."
        unless (null tcdFDs) $ do
          string " | "
          forM_ tcdFDs $ \(L _ (FunDep _ from to)) -> do
            spaced $ fmap pretty from
            string " -> "
            spaced $ fmap pretty to
        unless (null sigsAndMethods) $ string " where"
      verHead = do
        string "class "
        case tcdFixity of
          Prefix ->
            spaced $ (pretty tcdLName) : fmap output (hsq_explicit tcdTyVars)
          Infix ->
            case hsq_explicit tcdTyVars of
              (l:r:xs)
                -- TODO: Handle comments around 'tcdLName'.
               -> do
                parens $ spaced [output l, infixOp $ unLoc tcdLName, output r]
                forM_ xs $ \x -> do
                  space
                  output x
              _ -> error "Not enough parameters are given."
        unless (null tcdFDs) $ do
          newline
          indentedBlock $ do
            indentedDependingOnHead (string "| ") $
              prefixedLined ", " $
              flip fmap tcdFDs $ \(L _ (FunDep _ from to)) -> do
                spaced $ fmap pretty from
                string " -> "
                spaced $ fmap pretty to
          newline
          indentedBlock $ string "where"
        when (not (null sigsAndMethods) && null tcdFDs) $
          indentedBlock $ string " where"
      sigsAndMethods =
        sortByLocation $ fmap Sig tcdSigs ++ fmap Method (bagToList tcdMeths)
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Sig x)    = realSrcSpan $ locA $ getLoc x
      getLocation (Method x) = realSrcSpan $ locA $ getLoc x
  pretty' x = output x

instance Pretty (InstDecl GhcPs) where
  pretty' ClsInstD {..} = pretty cid_inst
  pretty' x             = output x

instance Pretty (HsBind GhcPs) where
  pretty' FunBind {..} = pretty fun_matches
  pretty' x            = output x
  commentsBefore FunBind {..} = commentsBefore fun_id
  commentsBefore _            = []
  commentsSameLine FunBind {..} = commentsSameLine fun_id
  commentsSameLine _            = Nothing
  commentsAfter FunBind {..} = commentsAfter fun_id
  commentsAfter _            = []

instance Pretty (Sig GhcPs) where
  pretty' (TypeSig _ funName params) = do
    pretty $ head funName
    horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        string " :: "
        pretty $ hswc_body params
      vertical =
        insideVerticalFunctionSignature $ do
          if isUsingForall
            then string " :: "
            else do
              string " ::"
              newline
          indentedBlock $ indentedWithSpace 3 $ pretty $ hswc_body params -- 3 for "-> "
      isUsingForall =
        case sig_bndrs (unLoc $ hswc_body params) of
          HsOuterExplicit {} -> True
          _                  -> False
  pretty' (ClassOpSig _ isDefault funName params) = do
    when isDefault $ string "default "
    output $ unLoc $ head funName
    string " :: "
    pretty $ sig_body $ unLoc params
  pretty' (MinimalSig _ _ xs) =
    indentedDependingOnHead (string "{-# MINIMAL ") $ do
      pretty xs
      string " #-}"
  pretty' x = output x

instance Pretty (HsDataDefn GhcPs) where
  pretty' HsDataDefn {..} =
    case dd_kindSig of
      Just kindSig -> do
        string " :: "
        output kindSig
        string " where"
        indentedBlock $
          forM_ dd_cons $ \x -> do
            newline
            pretty x
      Nothing ->
        indentedBlock $ do
          case length dd_cons of
            0 -> pure ()
            1 -> do
              string " ="
              newline
              pretty $ head dd_cons
            _ -> do
              newline
              indentedDependingOnHead (string "= ") $
                prefixedLined "| " $ fmap pretty dd_cons
          unless (null dd_derivs) $ do
            newline
            inter newline $ fmap pretty dd_derivs

instance Pretty (ClsInstDecl GhcPs) where
  pretty' ClsInstDecl {..} = do
    indentedDependingOnHead (string "instance ") $
      insideInstDecl $ pretty cid_poly_ty
    unless (isEmptyBag cid_binds) $ do
      string " where"
      newline
      indentedBlock $ mapM_ pretty cid_binds

instance Pretty (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' MG {..} = (inter newline $ pretty <$> unLoc mg_alts)

instance Pretty (HsExpr GhcPs) where
  pretty' v@HsVar {} = prefixExpr v
  pretty' HsUnboundVar {} = undefined
  pretty' HsConLikeOut {} = undefined
  pretty' HsRecFld {} = undefined
  pretty' (HsOverLabel _ l) = do
    string "#"
    output l
  pretty' HsIPVar {} = undefined
  pretty' full@HsOverLit {} = output full
  pretty' (HsLit _ l) = output l
  pretty' (HsLam _ body) = insideLambda $ pretty body
  pretty' (HsLamCase _ matches) =
    insideCase $ do
      string "\\case"
      if null $ unLoc $ mg_alts matches
        then string " {}"
        else do
          newline
          indentedBlock $ pretty matches
  pretty' (HsApp _ l r) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = spaced [pretty l, pretty r]
      vertical = do
        let (f:args) = flatten l ++ [r]
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
        indentedWithSpace spaces' (inter newline $ fmap pretty args)
      flatten :: LHsExpr GhcPs -> [LHsExpr GhcPs]
      flatten (L (SrcSpanAnn (EpAnn _ _ cs) _) (HsApp _ l' r')) =
        flatten l' ++ [insertComments cs r']
      flatten x = [x]
      insertComments :: EpAnnComments -> LHsExpr GhcPs -> LHsExpr GhcPs
      insertComments cs (L (s@SrcSpanAnn {ann = e@EpAnn {comments = cs'}}) r') =
        L (s {ann = e {comments = cs <> cs'}}) r'
      insertComments _ x = x
  pretty' t@HsAppType {} = output t
  pretty' (OpApp _ l o r) = pretty (InfixApp l o r False)
  pretty' NegApp {} = undefined
  pretty' (HsPar _ expr) = parens $ pretty expr
  pretty' SectionL {} = undefined
  pretty' SectionR {} = undefined
  pretty' (ExplicitTuple _ [] _) = string "()"
  pretty' (ExplicitTuple _ full _) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = parens $ inter (string ", ") $ fmap pretty full
      vertical =
        parens $
        prefixedLined "," $
        fmap
          (\e -> unless (isMissing e) (indentedDependingOnHead space $ pretty e))
          full
      -- vertical = do
      --   string "("
      --   unless (isMissing x) space
      --   pretty x
      --   forM_ xs $ \e -> do
      --     newline
      --     comma
      --     unless (isMissing e) space
      --     pretty e
      --   string ")"
      isMissing Missing {} = True
      isMissing _          = False
  pretty' ExplicitSum {} = undefined
  pretty' (HsCase _ cond arms) =
    insideCase $ do
      indentedDependingOnHead (string "case ") $ do
        pretty cond
        string " of"
      if null $ unLoc $ mg_alts arms
        then string " {}"
        else do
          newline
          indentedBlock $ pretty arms
  pretty' (HsIf _ cond t f) = do
    string "if "
    pretty cond
    indentedBlock $ do
      newline
      branch "then " t
      newline
      branch "else " f
    where
      branch :: String -> LHsExpr GhcPs -> Printer ()
      branch str e =
        case e of
          (L _ (HsDo _ DoExpr {} xs)) -> do
            string str
            string "do"
            newline
            indentedBlock $ inter newline $ fmap pretty $ unLoc xs -- TODO: Handle comments.
          _ -> indentedDependingOnHead (string str) $ pretty e
  pretty' (HsMultiIf _ guards) =
    indentedDependingOnHead (string "if ") $
    insideMultiwayIf $ inter newline $ fmap pretty guards
  pretty' (HsLet _ binds exprs) = do
    indentedDependingOnHead (string "let ") $ pretty binds
    newline
    indentedDependingOnHead (string " in ") $ pretty exprs
  pretty' (HsDo _ (DoExpr _) xs) =
    indentedDependingOnHead (string "do ") $ inter newline $ output <$> unLoc xs -- TODO: Handle comments.
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
  pretty' (HsDo _ MonadComp xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal =
        brackets $ do
          pretty $ head $ unLoc xs
          string " | "
          inter (string ", ") $ fmap pretty $ tail $ unLoc xs -- TODO: Handle comments.
      vertical =
        insideVerticalList $
        if null $ unLoc xs
          then string "[]"
          else let (lastStmt, others) = (head $ unLoc xs, tail $ unLoc xs)
                in do string "[ "
                      pretty lastStmt
                      newline
                      forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
                        indentedDependingOnHead (string p) $ pretty x
                        newline
                      string "]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)
  pretty' HsDo {} = undefined
  pretty' (ExplicitList _ xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = brackets $ inter (string ", ") $ fmap pretty xs
      vertical = do
        string "[ "
        inter (newline >> string ", ") $ fmap pretty xs
        newline
        string "]"
  pretty' (RecordCon _ name fields) =
    horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        name'
        space
        pretty fields
      vertical = do
        name'
        (space >> pretty fields) `ifFitsOnOneLineOrElse`
          (newline >> indentedBlock (pretty fields))
      name' =
        if head (showOutputable name) == ':'
          then parens $ output name
          else output name
  pretty' (RecordUpd _ name fields) = do
    pretty name
    space
    braces $
      -- TODO: Refactor this case.
      case fields of
        Right xs -> do
          forM_ xs $ \(L l HsRecField {..}) -> do
            printCommentsBefore l
            pretty hsRecFieldLbl
            string " = "
            pretty hsRecFieldArg
            printCommentsSameLine l
            printCommentsAfter l
        Left xs -> do
          forM_ xs $ \(L l HsRecField {..}) -> do
            printCommentsBefore l
            pretty hsRecFieldLbl
            string " = "
            pretty hsRecFieldArg
            printCommentsSameLine l
            printCommentsAfter l
  pretty' HsGetField {} = undefined
  pretty' HsProjection {} = undefined
  pretty' (ExprWithTySig _ e sig) = do
    pretty e
    string " :: "
    pretty $ hswc_body sig
  pretty' ArithSeq {} = undefined
  pretty' (HsBracket _ inner) = pretty inner
  pretty' HsRnBracketOut {} = undefined
  pretty' HsTcBracketOut {} = undefined
  pretty' (HsSpliceE _ x) = pretty x
  pretty' HsProc {} = undefined
  pretty' HsStatic {} = undefined
  pretty' HsTick {} = undefined
  pretty' HsBinTick {} = undefined
  pretty' HsPragE {} = undefined
  commentsBefore (HsVar _ x)   = commentsBefore x
  commentsBefore (HsApp x _ _) = commentsBefore x
  commentsBefore _             = []
  commentsSameLine (HsVar _ x)   = commentsSameLine x
  commentsSameLine (HsApp x _ _) = commentsSameLine x
  commentsSameLine _             = Nothing
  commentsAfter (HsVar _ x)   = commentsAfter x
  commentsAfter (HsApp x _ _) = commentsAfter x
  commentsAfter _             = []

instance Pretty (HsSigType GhcPs) where
  pretty' HsSig {..} = do
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap output xs
        isVertical <- gets psInsideVerticalFunctionSignature
        if isVertical
          then do
            string "."
            newline
          else string ". "
      _ -> return ()
    pretty sig_body

instance Pretty (ConDecl GhcPs) where
  pretty' ConDeclGADT {..} = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        output $ head con_names
        string " :: "
        pretty con_g_args
        string " -> "
        output con_res_ty
      vertical = do
        output $ head con_names
        newline
        indentedBlock $ do
          indentedDependingOnHead (string ":: ") $ pretty con_g_args
          newline
          string "-> "
          output con_res_ty
  pretty' ConDeclH98 {..}
    -- TODO: Refactor.
   = do
    if con_forall
      then indentedDependingOnHead
             (do string "forall "
                 spaced $ fmap output con_ex_tvs
                 string ". ")
      -- TODO: Handle comments.
             (do case con_mb_cxt of
                   Nothing -> return ()
                   Just (L _ []) -> return ()
                   Just (L _ [x]) -> do
                     pretty x
                     string " =>"
                     newline
                   Just (L _ xs) -> do
                     parens $ inter (string ", ") $ fmap pretty xs
                     string " =>"
                     newline
                 pretty con_name
                 pretty con_args)
      else do
        case con_args of
          (InfixCon l r) ->
            spaced [pretty l, infixOp $ unLoc con_name, pretty r] -- TODO: Handle comments.
          _ -> do
            pretty con_name
            pretty con_args

instance Pretty (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' Match {..} = do
    isInsideCase <- gets psInsideCase
    isInsideLambda <- gets psInsideLambda
    whenInsideLambda $ string "\\"
    case (isInsideCase, isInsideLambda, mc_fixity m_ctxt) of
      (True, _, _) -> do
        mapM_ pretty m_pats
        pretty m_grhss
      (_, True, _) -> do
        unless (null m_pats) $
          case unLoc $ head m_pats of
            LazyPat {} -> space
            BangPat {} -> space
            _          -> return ()
        spaced $ fmap pretty m_pats
        space
        pretty m_grhss
      (_, _, Prefix) -> do
        pretty m_ctxt
        unless (null m_pats) $
          forM_ m_pats $ \x -> do
            space
            pretty x
        pretty m_grhss
      (_, _, Infix) -> do
        case (m_pats, m_ctxt) of
          ((l:r:xs), FunRhs {..}) -> do
            pretty l
            space
            infixOp $ unLoc mc_fun
            space
            pretty r
            forM_ xs $ \x -> do
              space
              pretty x
            pretty m_grhss
          _ -> error "Not enough parameters are passed."
  commentsBefore Match {..} = commentsBefore m_ext
  commentsSameLine Match {..} = commentsSameLine m_ext
  commentsAfter Match {..} = commentsAfter m_ext

instance Pretty (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' l@LastStmt {} = output l
  pretty' full@(BindStmt _ pat body) =
    output full `ifFitsOnOneLineOrElse` do
      output pat
      string " <-"
      newline
      indentedBlock $ pretty body
  pretty' ApplicativeStmt {} = undefined
  pretty' (BodyStmt _ (L loc (OpApp _ l o r)) _ _) =
    pretty (L loc (InfixApp l o r True))
  pretty' (BodyStmt _ body _ _) = pretty body
  pretty' (LetStmt _ l) = do
    string "let "
    pretty l
  pretty' (ParStmt _ xs _ _) = do
    inVertical <- gets psInsideVerticalList
    if inVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string " | ") $ fmap output xs
      vertical = prefixedLined "| " $ fmap pretty xs
  pretty' TransStmt {..} =
    prefixedLined ", " $
    fmap pretty trS_stmts ++ [string "then " >> pretty trS_using]
  pretty' RecStmt {} = undefined
  commentsBefore (LetStmt l _) = commentsBefore l
  commentsBefore _             = []
  commentsAfter (LetStmt l _) = commentsAfter l
  commentsAfter _             = []

-- FIXME: Reconsider using a type variable. Using type variables may need
-- to define odd instances (e.g., Void).
instance Pretty a => Pretty (HsRecFields GhcPs a) where
  pretty' HsRecFields {..} = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal =
        braces $
        case rec_dotdot of
          Just _  -> string ".."
          Nothing -> inter (string ", ") $ fmap pretty rec_flds
      vertical = do
        indentedDependingOnHead (string "{ ") $ do
          prefixedLined ", " $ fmap pretty rec_flds
          newline
        string "}"

instance Pretty (HsType GhcPs) where
  pretty' HsForAllTy {} = undefined
  pretty' HsQualTy {..} = do
    isInSig <- gets psInsideSignature
    if isInSig
      then sigHor `ifFitsOnOneLineOrElse` sigVer
      else notInSig
    where
      sigHor = do
        constraints
        string " => "
        pretty hst_body
      sigVer = do
        constraints
        newline
        indentedWithSpace (-3) $ string "=> "
        pretty hst_body
      notInSig = do
        isInst <- gets psInsideInstDecl
        if isInst
          then notHor `ifFitsOnOneLineOrElse` notVer
          else notVer
      notHor = do
        constraints
        string " => "
        pretty hst_body
      notVer = do
        constraints
        string " =>"
        newline
        isInst <- gets psInsideInstDecl
        (if isInst
           then id
           else indentedBlock) $
          pretty hst_body
      constraints = hCon `ifFitsOnOneLineOrElse` vCon
      hCon =
        constraintsParens $
        mapM_ (inter (string ", ") . fmap pretty . unLoc) hst_ctxt -- TODO: Handle comments
      vCon = do
        string constraintsParensL
        space
        forM_ hst_ctxt $ \(L l cs) -> do
          printCommentsBefore l
          inter (newline >> string ", ") $ fmap pretty cs
          printCommentsSameLine l
          printCommentsAfter l
        newline
        string constraintsParensR
      -- TODO: Clean up here.
      constraintsParensL =
        case hst_ctxt of
          Nothing        -> ""
          Just (L _ [])  -> "("
          Just (L _ [_]) -> ""
          Just _         -> "("
      constraintsParensR =
        case hst_ctxt of
          Nothing        -> ""
          Just (L _ [])  -> ")"
          Just (L _ [_]) -> ""
          Just _         -> ")"
      constraintsParens =
        case hst_ctxt of
          Nothing        -> id
          Just (L _ [])  -> parens
          Just (L _ [_]) -> id
          Just _         -> parens
  pretty' x@HsTyVar {} = output x
  pretty' (HsAppTy _ l r) = do
    pretty l
    space
    pretty r
  pretty' HsAppKindTy {} = undefined
  pretty' (HsFunTy _ _ a b) = do
    isVertical <- gets psInsideVerticalFunctionSignature
    if isVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        pretty a
        string " -> "
        pretty b
      vertical = do
        exitVerticalFunctionSignature $ pretty a
        newline
        indentedWithSpace (-3) $ string "-> "
        pretty b
  pretty' (HsListTy _ xs) = brackets $ pretty xs
  pretty' (HsTupleTy _ _ xs) = hor `ifFitsOnOneLineOrElse` ver
    where
      hor = parens $ inter (string ", ") $ fmap pretty xs
      ver = do
        indentedDependingOnHead (string "( ") $
          prefixedLined ", " $ fmap pretty xs
        string ")"
  pretty' HsSumTy {} = undefined
  -- For `HsOpTy`, we do not need a single quote for the infix operator. An
  -- explicit promotion is necessary if there is a data constructor and
  -- a type with the same name. However, infix data constructors never
  -- share their names with types because types cannot contain symbols.
  -- Thus there is no ambiguity.
  pretty' (HsOpTy _ l op r) = do
    pretty l
    space
    infixOp $ unLoc op
    space
    pretty r
  pretty' (HsParTy _ inside) = parens $ pretty inside
  pretty' t@HsIParamTy {} = output t
  pretty' HsStarTy {} = undefined
  pretty' HsKindSig {} = undefined
  pretty' (HsSpliceTy _ sp) = pretty sp
  pretty' HsDocTy {} = undefined
  pretty' e@HsBangTy {} = output e
  pretty' HsRecTy {} = undefined
  pretty' (HsExplicitListTy _ _ xs) =
    case xs of
      [] -> string "'[]"
      _ -> do
        string "'[ "
        inter (string ", ") $ fmap pretty xs
        string "]"
  pretty' (HsExplicitTupleTy _ xs) = do
    string "'( "
    inter (string ", ") $ fmap pretty xs
    string ")"
  pretty' (HsTyLit _ x) = output x
  pretty' HsWildCardTy {} = undefined
  pretty' XHsType {} = undefined

instance Pretty (HsConDeclGADTDetails GhcPs) where
  pretty' (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> output x
  pretty' (RecConGADT xs) = do
    string "{ "
    inter (newline >> string ", ") $
      flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
        output $ head cd_fld_names
        string " :: "
        output cd_fld_type
    string "}"

instance Pretty (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' GRHSs {..} = do
    mapM_ pretty grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds _ lr) ->
        indentedBlock $ do
          newline
          isCase <- gets psInsideCase
          if isCase
            then indentedDependingOnHead (string "where ") $
                 exitCase $ pretty lr
            else do
              string "where"
              newline
              indentedBlock $ pretty lr
      _ -> return ()

instance Pretty (HsMatchContext GhcPs) where
  pretty' FunRhs {..} = pretty mc_fun
  pretty' CaseAlt     = return ()
  pretty' LambdaExpr  = return ()
  pretty' x           = output x

instance Pretty (ParStmtBlock GhcPs GhcPs) where
  pretty' (ParStmtBlock _ xs _ _) = do
    inVertical <- gets psInsideVerticalList
    if inVertical
      then vertical
      else horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string ", ") $ fmap pretty xs
      vertical = prefixedLined ", " $ fmap pretty xs

instance Pretty RdrName where
  pretty' = prefixOp

instance Pretty (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  pretty' (GRHS _ [] (L _ (HsDo _ (DoExpr _) body))) = do
    unlessInsideLambda space
    rhsSeparator
    space
    string "do"
    newline
    indentedBlock $ do
      printCommentsBefore $ getLoc body
      inter newline $ pretty <$> unLoc body
      printCommentsSameLine $ getLoc body
      printCommentsAfter $ getLoc body
  pretty' (GRHS _ guards (L _ (HsDo _ (DoExpr _) body))) = do
    isInsideMultiwayIf <- gets psInsideMultiwayIf
    unless isInsideMultiwayIf newline
    indentedBlock $ do
      string "| "
      inter
        (if isInsideMultiwayIf
           then comma >> newline
           else newline >> string ", ") $
        fmap pretty guards
      space
      rhsSeparator
      string " do "
      mapM_ pretty $ unLoc body
  pretty' (GRHS _ [] body) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        unlessInsideLambda space
        rhsSeparator
        space
        pretty body
      vertical = do
        unlessInsideLambda space
        rhsSeparator
        newline
        exitLambda $ indentedBlock $ pretty body
  pretty' (GRHS _ guards body) = do
    isInsideMultiwayIf <- gets psInsideMultiwayIf
    unless isInsideMultiwayIf newline
    (if isInsideMultiwayIf
       then id
       else indentedBlock) $
      (if isInsideMultiwayIf
         then indentedDependingOnHead (string "| ")
         else (string "| " >>)) $ do
        inter
          (if isInsideMultiwayIf
             then comma >> newline
             else newline >> string ", ") $
          fmap pretty guards
        horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        space
        rhsSeparator
        space
        pretty body
      vertical = do
        isInsideMultiwayIf <- gets psInsideMultiwayIf
        space
        rhsSeparator
        newline
        (if isInsideMultiwayIf
           then id
           else indentedBlock) $
          pretty body
  commentsBefore (GRHS x _ _) = commentsBefore x
  commentsSameLine (GRHS x _ _) = commentsSameLine x
  commentsAfter (GRHS x _ _) = commentsAfter x

instance Pretty EpaCommentTok where
  pretty' (EpaLineComment c)    = string c
  pretty' (EpaBlockComment c)   = string c
  pretty' (EpaDocCommentPrev c) = string ("-- ^" ++ c)
  pretty' (EpaDocCommentNext c) = string ("-- |" ++ c)
  pretty' _                     = undefined

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
  pretty' p@HsQuasiQuote {} = output p
  pretty' HsSpliced {} = undefined

instance Pretty (Pat GhcPs) where
  pretty' p@WildPat {} = output p
  pretty' p@VarPat {} = output p
  pretty' p@LazyPat {} = output p
  pretty' AsPat {} = undefined
  pretty' (ParPat _ inner) = parens $ pretty inner
  pretty' p@BangPat {} = output p
  pretty' ListPat {} = undefined
  pretty' (TuplePat _ pats _) = parens $ inter (string ", ") $ fmap pretty pats
  pretty' SumPat {} = undefined
  pretty' ConPat {..} =
    case pat_args of
      PrefixCon _ as -> do
        prefixOp $ unLoc pat_con
        mapM_ (\x -> space >> pretty x) as
      RecCon rec ->
        indentedDependingOnHead (pretty pat_con >> space) $ pretty rec
      InfixCon a b -> do
        pretty a
        unlessSpecialOp (unLoc pat_con) space
        infixOp $ unLoc pat_con
        unlessSpecialOp (unLoc pat_con) space
        pretty b
  pretty' ViewPat {} = undefined
  pretty' p@SplicePat {} = output p
  pretty' p@LitPat {} = output p
  pretty' NPat {} = undefined
  pretty' p@NPlusKPat {} = output p
  pretty' SigPat {} = undefined

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

instance Pretty SigOrMethods where
  pretty' (Sig x)    = pretty x
  pretty' (Method x) = pretty x

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
  commentsSameLine (SrcSpanAnn ep _) = commentsSameLine ep
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
  commentsSameLine (EpAnn ann _ cs) = find isSameLine $ getFollowingComments cs
    where
      isSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  commentsSameLine EpAnnNotUsed = Nothing
  commentsAfter (EpAnn ann _ cs) =
    filter (not . isSameLine) $ getFollowingComments cs
    where
      isSameLine (L comAnn _) =
        srcSpanEndLine (anchor ann) == srcSpanStartLine (anchor comAnn)
  commentsAfter EpAnnNotUsed = []

instance Pretty (HsLocalBindsLR GhcPs GhcPs) where
  pretty' (HsValBinds _ lr) = pretty lr
  pretty' x                 = output x

instance Pretty (HsValBindsLR GhcPs GhcPs) where
  pretty' (ValBinds _ methods sigs) = inter newline $ fmap pretty sigsAndMethods
      -- TODO: Merge this where clause with the one in the 'ClassDecl' of
      -- 'TyClDecl'.
    where
      sigsAndMethods =
        sortByLocation $ fmap Sig sigs ++ fmap Method (bagToList methods)
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Sig x)    = realSrcSpan $ locA $ getLoc x
      getLocation (Method x) = realSrcSpan $ locA $ getLoc x
  pretty' x = output x

instance Pretty (HsTupArg GhcPs) where
  pretty' (Present _ e) = pretty e
  pretty' Missing {}    = return () -- This appears in a tuple section.

-- FIXME: Reconsider using a type variable. Using type variables may need
-- to define odd instances (e.g., Void).
instance (Pretty a, Pretty b) => Pretty (HsRecField' a b) where
  pretty' HsRecField {..} = horizontal `ifFitsOnOneLineOrElse` vertical
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

instance Pretty (FieldOcc GhcPs) where
  pretty' FieldOcc {..} = pretty rdrNameFieldOcc

-- HsConDeclH98Details
instance Pretty (HsConDetails Void (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs))) (GenLocated SrcSpanAnnL [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  pretty' (PrefixCon _ xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal =
        forM_ xs $ \x -> do
          space
          pretty x
      vertical =
        indentedBlock $
        forM_ xs $ \x -> do
          newline
          pretty x
  pretty' (RecCon (L _ rec)) =
    indentedBlock $ do
      newline
      string "{ "
      inter (newline >> string ", ") $ fmap pretty rec
      newline
      string "}"
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
    pretty $ head cd_fld_names
    string " :: "
    pretty cd_fld_type

instance Pretty InfixExpr where
  pretty' (InfixExpr (L _ (HsVar _ bind))) = infixOp $ unLoc bind
  pretty' (InfixExpr x)                    = pretty' x
  commentsBefore (InfixExpr x) = commentsBefore x
  commentsSameLine (InfixExpr x) = commentsSameLine x
  commentsAfter (InfixExpr x) = commentsAfter x

instance Pretty InfixApp where
  pretty' InfixApp {..} = horizontal `ifFitsOnOneLineOrElse` vertical
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
        (if immediatelyAfterDo
           then indentedBlock
           else id) $ do
          case unLoc rhs of
            (HsDo _ (DoExpr _) xs) -> do
              string " do"
              newline
              indentedBlock $ inter newline $ fmap pretty $ unLoc xs -- TODO: Handle comments.
            HsLam {} -> do
              space
              pretty rhs
            HsLamCase {} -> do
              space
              pretty rhs
            _ -> do
              beforeRhs
              pretty rhs
      lhsVer =
        case lhs of
          (L loc (OpApp _ l o r)) ->
            pretty (L loc (InfixApp l o r immediatelyAfterDo))
          _ -> pretty lhs

instance Pretty a => Pretty (BooleanFormula a) where
  pretty' (Var x) = pretty x
  pretty' (And xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string ", ") $ fmap pretty xs
      vertical = prefixedLined ", " $ fmap pretty xs
  pretty' (Or xs) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = inter (string " | ") $ fmap pretty xs
      vertical = prefixedLined "| " $ fmap pretty xs
  pretty' (Parens x) = parens $ pretty x

instance Pretty (FieldLabelStrings GhcPs) where
  pretty' _ = undefined

instance Pretty (AmbiguousFieldOcc GhcPs) where
  pretty' (Unambiguous _ name) = pretty name
  pretty' (Ambiguous _ name)   = pretty name

instance Pretty (ImportDecl GhcPs) where
  pretty' ImportDecl {..} = do
    string "import "
    when (ideclSource == IsBoot) $ string "{-# SOURCE #-} "
    when ideclSafe $ string "safe "
    unless (ideclQualified == NotQualified) $ string "qualified "
    output ideclName
    whenJust ideclAs $ \x -> do
      string " as "
      output x
    whenJust ideclHiding $ \(x, _) -> do
      when x (string " hiding")
      (string " " >> horizontalTuple explicitOrHidingImports) `ifFitsOnOneLineOrElse`
        (newline >> indentedBlock (verticalTuple explicitOrHidingImports))
    where
      explicitOrHidingImports =
        output <$> maybe [] (fmap unLoc . unLoc . snd) ideclHiding

instance Pretty (HsDerivingClause GhcPs) where
  pretty' HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      output x
      space
    pretty deriv_clause_tys

instance Pretty (DerivClauseTys GhcPs) where
  pretty' (DctSingle _ ty) = do
    parens $ pretty ty
  pretty' (DctMulti _ ts) = horizontal `ifFitsOnOneLineOrElse` vertical
    where
      horizontal = do
        parens $ inter (string ", ") $ fmap pretty ts
      vertical = do
        indentedDependingOnHead (string "( ") $ do
          prefixedLined ", " $ fmap pretty ts
          newline
          indentedWithSpace (-2) $ string ")"

prefixExpr :: HsExpr GhcPs -> Printer ()
prefixExpr (HsVar _ bind) = prefixOp $ unLoc bind
prefixExpr x              = pretty x
