{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Pretty printing.
module HIndent.PrettyPrint
  ( prettyPrint
  ) where

import           Control.Monad
import           Control.Monad.RWS
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Void
import           Generics.SYB                           hiding (Infix, Prefix)
import           GHC.Core.InstEnv
import           GHC.Data.Bag
import           GHC.Data.BooleanFormula
import           GHC.Hs
import           GHC.Types.Fixity
import           GHC.Types.Name.Reader
import           GHC.Types.SourceText
import           GHC.Types.SrcLoc
import           GHC.Unit
import           HIndent.Applicative
import           HIndent.PrettyPrint.Combinators
import           HIndent.PrettyPrint.Combinators.Indent
import           HIndent.PrettyPrint.Combinators.Inside
import           HIndent.PrettyPrint.Combinators.Lineup
import           HIndent.PrettyPrint.Combinators.Op
import           HIndent.PrettyPrint.Combinators.String
import           HIndent.PrettyPrint.Combinators.Wrap
import           HIndent.PrettyPrint.Imports.Sort
import           HIndent.PrettyPrint.ModuleDeclaration
import           HIndent.PrettyPrint.Pragma
import           HIndent.Types

data SigMethodsFamily
  = Sig (LSig GhcPs)
  | Method (LHsBindLR GhcPs GhcPs)
  | TypeFamily (LFamilyDecl GhcPs)

newtype InfixExpr =
  InfixExpr (LHsExpr GhcPs)

data InfixApp =
  InfixApp
    { lhs                :: LHsExpr GhcPs
    , op                 :: LHsExpr GhcPs
    , rhs                :: LHsExpr GhcPs
    , immediatelyAfterDo :: Bool
    }

-- | This function prettyPrint-prints the given AST node with comments.
prettyPrint :: PrettyPrintable a => a -> Printer ()
prettyPrint p = do
  printCommentsBefore p
  prettyPrint' p
  printCommentsSameLine p
  printCommentsAfter p

printCommentsBefore :: PrettyPrintable a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore p) $ \x -> do
    prettyPrint x
    newline

printCommentsSameLine :: PrettyPrintable a => a -> Printer ()
printCommentsSameLine (commentOnSameLine -> Just (L sp c)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithLevel (fromIntegral $ srcSpanStartCol $ anchor sp) $
         prettyPrint c
    else do
      space
      prettyPrint c
  eolCommentsArePrinted
printCommentsSameLine _ = return ()

printCommentsAfter :: PrettyPrintable a => a -> Printer ()
printCommentsAfter p =
  case commentsAfter p of
    [] -> return ()
    xs -> do
      isThereCommentsOnSameLine <- gets psEolComment
      unless isThereCommentsOnSameLine newline
      forM_ xs $ \(L loc c) -> do
        let col = fromIntegral $ srcSpanStartCol (anchor loc) - 1
        indentedWithLevel col $ prettyPrint c
        eolCommentsArePrinted

-- | Pretty print including comments.
--
-- FIXME: 'Pretty' has a problem. It has two responsibilities; one is to
-- print a given node prettyPrint, and the other is to collect comments from the
-- node.
--
-- Note that there are three types of nodes:
-- * A node that can prettyPrint-print and has comments (e.g., 'HsModule')
-- * A node that can prettyPrint-print but has no comments (e.g., almost all
-- nodes)
-- * A node that cannot prettyPrint-print but has comments (e.g., 'EpAnn')
class PrettyPrintable a where
  prettyPrint' :: a -> Printer ()
  -- These functions must return comments that only this node can fetch. In
  -- other words, these functions must not return comments that child nodes
  -- can fetch.
  commentsBefore :: a -> [LEpaComment]
  commentsBefore = const []
  commentOnSameLine :: a -> Maybe LEpaComment
  commentOnSameLine = const Nothing
  commentsAfter :: a -> [LEpaComment]
  commentsAfter = const []

instance PrettyPrintable HsModule where
  prettyPrint' m = blanklined printers
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
        mapM_ (\(x, sp) -> prettyPrint x >> fromMaybe (return ()) sp) $
        addSeparator $ hsmodDecls m
      addSeparator []     = []
      addSeparator [x]    = [(x, Nothing)]
      addSeparator (x:xs) = (x, Just $ separator $ unLoc x) : addSeparator xs
      separator SigD {} = newline
      separator _       = blankline
      declsExist = not . null . hsmodDecls
      outputImports =
        blanklined .
        fmap (outputImportGroup . sortImportsByName) .
        groupImports . sortImportsByLocation . hsmodImports
      outputImportGroup = lined . fmap prettyPrint
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
-- (e.g., 'EpAnn') cannot prettyPrint-print. The restriction exists only for
-- extracting comments. Remove the restriction.
instance (PrettyPrintable l, PrettyPrintable e) =>
         PrettyPrintable (GenLocated l e) where
  prettyPrint' (L _ e) = prettyPrint e
  commentsBefore (L l _) = commentsBefore l
  commentOnSameLine (L l _) = commentOnSameLine l
  commentsAfter (L l _) = commentsAfter l

instance PrettyPrintable (HsDecl GhcPs) where
  prettyPrint' (TyClD _ d)    = prettyPrint d
  prettyPrint' (InstD _ inst) = prettyPrint inst
  prettyPrint' DerivD {}      = undefined
  prettyPrint' (ValD _ bind)  = prettyPrint bind
  prettyPrint' (SigD _ s)     = insideSignature $ prettyPrint s
  prettyPrint' KindSigD {}    = undefined
  prettyPrint' DefD {}        = undefined
  prettyPrint' x@ForD {}      = output x
  prettyPrint' WarningD {}    = undefined
  prettyPrint' AnnD {}        = undefined
  prettyPrint' RuleD {}       = undefined
  prettyPrint' (SpliceD _ sp) = prettyPrint sp
  prettyPrint' DocD {}        = return ()
  prettyPrint' RoleAnnotD {}  = undefined

instance PrettyPrintable (TyClDecl GhcPs) where
  prettyPrint' SynDecl {..} = do
    string "type "
    -- TODO: Merge this case with the one in 'ClassDecl's branch.
    case tcdFixity of
      Prefix ->
        spaced $ prettyPrint tcdLName : fmap output (hsq_explicit tcdTyVars)
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
    hor <-|> ver
    where
      hor = do
        string " = "
        prettyPrint tcdRhs
      ver =
        indentedWithSpace 3 $ do
          newline
          string "= "
          indentedBlock $ prettyPrint tcdRhs
  prettyPrint' DataDecl {..} = do
    case dd_ND tcdDataDefn of
      DataType -> string "data "
      NewType  -> string "newtype "
    output tcdLName
    forM_ (hsq_explicit tcdTyVars) $ \x -> do
      space
      output x
    prettyPrint tcdDataDefn
  prettyPrint' ClassDecl {..} = do
    if isJust tcdCtxt
      then verHead
      else horHead <-|> verHead
    newline
    indentedBlock $ lined $ fmap prettyPrint sigsMethodsFamilies
    where
      horHead = do
        string "class "
        case tcdFixity of
          Prefix ->
            spaced $ prettyPrint tcdLName : fmap output (hsq_explicit tcdTyVars)
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
            spaced $ fmap prettyPrint from
            string " -> "
            spaced $ fmap prettyPrint to
        unless (null sigsMethodsFamilies) $ string " where"
      verHead = do
        indentedDependingOnHead (string "class ") $ do
          whenJust tcdCtxt $ \(L _ xs) ->
            case xs -- TODO: Handle comments.
                  of
              [] -> undefined
              [x] -> do
                prettyPrint x
                string " =>"
                newline
              _ -> do
                hTuple $ fmap prettyPrint xs
                string " =>"
                newline
          case tcdFixity of
            Prefix ->
              spaced $
              prettyPrint tcdLName : fmap output (hsq_explicit tcdTyVars)
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
          indentedBlock $
            indentedDependingOnHead (string "| ") $
            vCommaSep $
            flip fmap tcdFDs $ \(L _ (FunDep _ from to)) -> do
              spaced $ fmap prettyPrint from
              string " -> "
              spaced $ fmap prettyPrint to
          newline
          indentedBlock $ string "where"
        when (isJust tcdCtxt) $ do
          newline
          indentedBlock $ string "where"
        when
          (not (null sigsMethodsFamilies) && null tcdFDs && isNothing tcdCtxt) $
          indentedBlock $ string " where"
      sigsMethodsFamilies =
        sortByLocation $
        fmap Sig tcdSigs ++
        fmap Method (bagToList tcdMeths) ++ fmap TypeFamily tcdATs
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Sig x)        = realSrcSpan $ locA $ getLoc x
      getLocation (Method x)     = realSrcSpan $ locA $ getLoc x
      getLocation (TypeFamily x) = realSrcSpan $ locA $ getLoc x
  prettyPrint' x = output x

instance PrettyPrintable (InstDecl GhcPs) where
  prettyPrint' ClsInstD {..} = prettyPrint cid_inst
  prettyPrint' x             = output x

instance PrettyPrintable (HsBind GhcPs) where
  prettyPrint' FunBind {..} = prettyPrint fun_matches
  prettyPrint' x            = output x
  commentsBefore FunBind {..} = commentsBefore fun_id
  commentsBefore _            = []
  commentOnSameLine FunBind {..} = commentOnSameLine fun_id
  commentOnSameLine _            = Nothing
  commentsAfter FunBind {..} = commentsAfter fun_id
  commentsAfter _            = []

instance PrettyPrintable (Sig GhcPs) where
  prettyPrint' (TypeSig _ funName params) = do
    prettyPrint $ head funName
    horizontal <-|> vertical
    where
      horizontal = do
        string " :: "
        prettyPrint $ hswc_body params
      vertical =
        insideVerticalFunctionSignature $ do
          if isUsingForall
            then string " :: "
            else do
              string " ::"
              newline
          indentedBlock $ indentedWithSpace 3 $ prettyPrint $ hswc_body params -- 3 for "-> "
      isUsingForall =
        case sig_bndrs (unLoc $ hswc_body params) of
          HsOuterExplicit {} -> True
          _                  -> False
  prettyPrint' (ClassOpSig _ isDefault funNames params) = do
    when isDefault $ string "default "
    hCommaSep $ fmap prettyPrint funNames
    string " :: "
    prettyPrint $ sig_body $ unLoc params
  prettyPrint' (MinimalSig _ _ xs) =
    indentedDependingOnHead (string "{-# MINIMAL ") $ do
      prettyPrint xs
      string " #-}"
  prettyPrint' x = output x

instance PrettyPrintable (HsDataDefn GhcPs) where
  prettyPrint' HsDataDefn {..} =
    case dd_kindSig of
      Just kindSig -> do
        string " :: "
        output kindSig
        string " where"
        indentedBlock $ newlinePrefixed $ fmap prettyPrint dd_cons
      Nothing ->
        indentedBlock $ do
          case length dd_cons of
            0 -> pure ()
            1 -> do
              string " ="
              newline
              prettyPrint $ head dd_cons
            _ -> do
              newline
              indentedDependingOnHead (string "= ") $
                vBarSep $ fmap prettyPrint dd_cons
          unless (null dd_derivs) $ do
            newline
            lined $ fmap prettyPrint dd_derivs

instance PrettyPrintable (ClsInstDecl GhcPs) where
  prettyPrint' ClsInstDecl {..} = do
    indentedDependingOnHead (string "instance ") $
      insideInstDecl $ do
        whenJust cid_overlap_mode $ \x -> do
          prettyPrint x
          space
        prettyPrint cid_poly_ty
    unless (isEmptyBag cid_binds) $ do
      string " where"
      newline
      indentedBlock $ mapM_ prettyPrint cid_binds

instance PrettyPrintable (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint' MG {..} = lined $ prettyPrint <$> unLoc mg_alts

instance PrettyPrintable (HsExpr GhcPs) where
  prettyPrint' v@HsVar {} = prefixExpr v
  prettyPrint' HsUnboundVar {} = undefined
  prettyPrint' HsConLikeOut {} = undefined
  prettyPrint' HsRecFld {} = undefined
  prettyPrint' (HsOverLabel _ l) = do
    string "#"
    output l
  prettyPrint' HsIPVar {} = undefined
  prettyPrint' full@HsOverLit {} = output full
  prettyPrint' (HsLit _ l) = output l
  prettyPrint' (HsLam _ body) = insideLambda $ prettyPrint body
  prettyPrint' (HsLamCase _ matches) =
    insideCase $ do
      string "\\case"
      if null $ unLoc $ mg_alts matches
        then string " {}"
        else do
          newline
          indentedBlock $ prettyPrint matches
  prettyPrint' (HsApp _ l r) = horizontal <-|> vertical
    where
      horizontal = spaced [prettyPrint l, prettyPrint r]
      vertical = do
        let (f, args) =
              case flatten l ++ [r] of
                (f':args') -> (f', args')
                _          -> error "Invalid function application."
        col <- gets psColumn
        spaces <- getIndentSpaces
        prettyPrint f
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
        indentedWithSpace spaces' $ lined $ fmap prettyPrint args
      flatten :: LHsExpr GhcPs -> [LHsExpr GhcPs]
      flatten (L (SrcSpanAnn (EpAnn _ _ cs) _) (HsApp _ l' r')) =
        flatten l' ++ [insertComments cs r']
      flatten x = [x]
      insertComments :: EpAnnComments -> LHsExpr GhcPs -> LHsExpr GhcPs
      insertComments cs (L s@SrcSpanAnn {ann = e@EpAnn {comments = cs'}} r') =
        L (s {ann = e {comments = cs <> cs'}}) r'
      insertComments _ x = x
  prettyPrint' t@HsAppType {} = output t
  prettyPrint' (OpApp _ l o r) = prettyPrint (InfixApp l o r False)
  prettyPrint' NegApp {} = undefined
  prettyPrint' (HsPar _ expr) = parens $ prettyPrint expr
  prettyPrint' (SectionL _ l o) =
    spaced [prettyPrint l, prettyPrint (InfixExpr o)]
  prettyPrint' (SectionR _ o r) =
    spaced [prettyPrint (InfixExpr o), prettyPrint r]
  prettyPrint' (ExplicitTuple _ full _) = horizontal <-|> vertical
    where
      horizontal = hTuple $ fmap prettyPrint full
      vertical =
        parens $
        prefixedLined "," $
        fmap
          (\e ->
             unless
               (isMissing e)
               (indentedDependingOnHead space $ prettyPrint e))
          full
      isMissing Missing {} = True
      isMissing _          = False
  prettyPrint' ExplicitSum {} = undefined
  prettyPrint' (HsCase _ cond arms) =
    insideCase $ do
      indentedDependingOnHead (string "case ") $ do
        prettyPrint cond
        string " of"
      if null $ unLoc $ mg_alts arms
        then string " {}"
        else do
          newline
          indentedBlock $ prettyPrint arms
  prettyPrint' (HsIf _ cond t f) = do
    string "if "
    prettyPrint cond
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
            indentedBlock $ lined $ prettyPrint <$> unLoc xs -- TODO: Handle comments.
          _ -> indentedDependingOnHead (string str) $ prettyPrint e
  prettyPrint' (HsMultiIf _ guards) =
    indentedDependingOnHead (string "if ") $
    insideMultiwayIf $ lined $ fmap prettyPrint guards
  prettyPrint' (HsLet _ binds exprs) = do
    indentedDependingOnHead (string "let ") $ prettyPrint binds
    newline
    indentedDependingOnHead (string " in ") $ prettyPrint exprs
  prettyPrint' (HsDo _ (DoExpr _) xs) =
    indentedDependingOnHead (string "do ") $ lined $ output <$> unLoc xs -- TODO: Handle comments.
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
  prettyPrint' (HsDo _ MonadComp xs) = horizontal <-|> vertical
    where
      horizontal =
        brackets $ do
          prettyPrint $ head $ unLoc xs
          string " | "
          hCommaSep $ fmap prettyPrint $ tail $ unLoc xs -- TODO: Handle comments.
      vertical =
        insideVerticalList $
        if null $ unLoc xs
          then string "[]"
          else let (lastStmt, others) = (head $ unLoc xs, tail $ unLoc xs)
                in do string "[ "
                      prettyPrint lastStmt
                      newline
                      forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
                        indentedDependingOnHead (string p) $ prettyPrint x
                        newline
                      string "]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)
  prettyPrint' HsDo {} = undefined
  prettyPrint' (ExplicitList _ xs) = horizontal <-|> vertical
    where
      horizontal = brackets $ hCommaSep $ fmap prettyPrint xs
      vertical = vList $ fmap prettyPrint xs
  prettyPrint' (RecordCon _ name fields) = horizontal <-|> vertical
    where
      horizontal = do
        name'
        space
        prettyPrint fields
      vertical = do
        name'
        (space >> prettyPrint fields) <-|>
          (newline >> indentedBlock (prettyPrint fields))
      name' =
        if head (showOutputable name) == ':'
          then parens $ output name
          else output name
  prettyPrint' (RecordUpd _ name fields) = do
    prettyPrint name
    space
    braces $
      -- TODO: Refactor this case.
      case fields of
        Right xs ->
          forM_ xs $ \(L l HsRecField {..}) -> do
            printCommentsBefore l
            prettyPrint hsRecFieldLbl
            string " = "
            prettyPrint hsRecFieldArg
            printCommentsSameLine l
            printCommentsAfter l
        Left xs ->
          forM_ xs $ \(L l HsRecField {..}) -> do
            printCommentsBefore l
            prettyPrint hsRecFieldLbl
            string " = "
            prettyPrint hsRecFieldArg
            printCommentsSameLine l
            printCommentsAfter l
  prettyPrint' HsGetField {} = undefined
  prettyPrint' HsProjection {} = undefined
  prettyPrint' (ExprWithTySig _ e sig) = do
    prettyPrint e
    string " :: "
    prettyPrint $ hswc_body sig
  prettyPrint' (ArithSeq _ _ x) = prettyPrint x
  prettyPrint' (HsBracket _ inner) = prettyPrint inner
  prettyPrint' HsRnBracketOut {} = undefined
  prettyPrint' HsTcBracketOut {} = undefined
  prettyPrint' (HsSpliceE _ x) = prettyPrint x
  prettyPrint' HsProc {} = undefined
  prettyPrint' HsStatic {} = undefined
  prettyPrint' HsTick {} = undefined
  prettyPrint' HsBinTick {} = undefined
  prettyPrint' HsPragE {} = undefined
  commentsBefore (HsVar _ x)   = commentsBefore x
  commentsBefore (HsApp x _ _) = commentsBefore x
  commentsBefore _             = []
  commentOnSameLine (HsVar _ x)   = commentOnSameLine x
  commentOnSameLine (HsApp x _ _) = commentOnSameLine x
  commentOnSameLine _             = Nothing
  commentsAfter (HsVar _ x)   = commentsAfter x
  commentsAfter (HsApp x _ _) = commentsAfter x
  commentsAfter _             = []

instance PrettyPrintable (HsSigType GhcPs) where
  prettyPrint' HsSig {..} = do
    case sig_bndrs of
      HsOuterExplicit _ xs -> do
        string "forall "
        spaced $ fmap output xs
        isVertical <- gets ((InsideVerticalFunctionSignature `elem`) . psInside)
        if isVertical
          then do
            string "."
            newline
          else string ". "
      _ -> return ()
    prettyPrint sig_body

instance PrettyPrintable (ConDecl GhcPs) where
  prettyPrint' ConDeclGADT {..} = horizontal <-|> vertical
    where
      horizontal = do
        output $ head con_names
        string " :: "
        prettyPrint con_g_args
        string " -> "
        output con_res_ty
      vertical = do
        output $ head con_names
        newline
        indentedBlock $ do
          indentedDependingOnHead (string ":: ") $ prettyPrint con_g_args
          newline
          string "-> "
          output con_res_ty
  prettyPrint' ConDeclH98 {..}
    -- TODO: Refactor.
   =
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
                     prettyPrint x
                     string " =>"
                     newline
                   Just (L _ xs) -> do
                     hTuple $ fmap prettyPrint xs
                     string " =>"
                     newline
                 prettyPrint con_name
                 prettyPrint con_args)
      else do
        case con_args of
          (InfixCon l r) ->
            spaced [prettyPrint l, infixOp $ unLoc con_name, prettyPrint r] -- TODO: Handle comments.
          _ -> do
            prettyPrint con_name
            prettyPrint con_args

instance PrettyPrintable (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint' Match {..} = do
    isInsideCase <- gets ((InsideCase `elem`) . psInside)
    isInsideLambda <- gets ((InsideLambda `elem`) . psInside)
    whenInsideLambda $ string "\\"
    case (isInsideCase, isInsideLambda, mc_fixity m_ctxt) of
      (True, _, _) -> do
        mapM_ prettyPrint m_pats
        prettyPrint m_grhss
      (_, True, _) -> do
        unless (null m_pats) $
          case unLoc $ head m_pats of
            LazyPat {} -> space
            BangPat {} -> space
            _          -> return ()
        spaced $ fmap prettyPrint m_pats ++ [prettyPrint m_grhss]
      (_, _, Prefix) -> do
        prettyPrint m_ctxt
        unless (null m_pats) $ do
          space
          spaced $ fmap prettyPrint m_pats
        prettyPrint m_grhss
      (_, _, Infix) -> do
        case (m_pats, m_ctxt) of
          (l:r:xs, FunRhs {..}) -> do
            spaced $
              [prettyPrint l, infixOp $ unLoc mc_fun, prettyPrint r] ++
              fmap prettyPrint xs
            prettyPrint m_grhss
          _ -> error "Not enough parameters are passed."
  commentsBefore Match {..} = commentsBefore m_ext
  commentOnSameLine Match {..} = commentOnSameLine m_ext
  commentsAfter Match {..} = commentsAfter m_ext

instance PrettyPrintable (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint' l@LastStmt {} = output l
  prettyPrint' full@(BindStmt _ pat body) =
    output full <-|> do
      output pat
      string " <-"
      newline
      indentedBlock $ prettyPrint body
  prettyPrint' ApplicativeStmt {} = undefined
  prettyPrint' (BodyStmt _ (L loc (OpApp _ l o r)) _ _) =
    prettyPrint (L loc (InfixApp l o r True))
  prettyPrint' (BodyStmt _ body _ _) = prettyPrint body
  prettyPrint' (LetStmt _ l) = do
    string "let "
    prettyPrint l
  prettyPrint' (ParStmt _ xs _ _) = do
    inVertical <- gets ((InsideVerticalList `elem`) . psInside)
    if inVertical
      then vertical
      else horizontal <-|> vertical
    where
      horizontal = hBarSep $ fmap output xs
      vertical = vBarSep $ fmap prettyPrint xs
  prettyPrint' TransStmt {..} =
    vCommaSep $
    fmap prettyPrint trS_stmts ++ [string "then " >> prettyPrint trS_using]
  prettyPrint' RecStmt {} = undefined
  commentsBefore (LetStmt l _) = commentsBefore l
  commentsBefore _             = []
  commentsAfter (LetStmt l _) = commentsAfter l
  commentsAfter _             = []

-- FIXME: Reconsider using a type variable. Using type variables may need
-- to define odd instances (e.g., Void).
instance PrettyPrintable a => PrettyPrintable (HsRecFields GhcPs a) where
  prettyPrint' HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _  -> braces $ string ".."
          Nothing -> hFields $ fmap prettyPrint rec_flds
      vertical = vFields $ fmap prettyPrint rec_flds

instance PrettyPrintable (HsType GhcPs) where
  prettyPrint' HsForAllTy {} = undefined
  prettyPrint' HsQualTy {..} = do
    isInSig <- gets ((InsideSignature `elem`) . psInside)
    if isInSig
      then sigHor <-|> sigVer
      else notInSig
    where
      sigHor = do
        constraints
        string " => "
        prettyPrint hst_body
      sigVer = do
        constraints
        newline
        indentedWithSpace (-3) $ string "=> "
        prettyPrint hst_body
      notInSig = do
        isInst <- gets ((InsideInstDecl `elem`) . psInside)
        if isInst
          then notHor <-|> notVer
          else notVer
      notHor = do
        constraints
        string " => "
        prettyPrint hst_body
      notVer = do
        constraints
        string " =>"
        newline
        isInst <- gets ((InsideInstDecl `elem`) . psInside)
        (if isInst
           then id
           else indentedBlock) $
          prettyPrint hst_body
      constraints = hCon <-|> vCon
      hCon =
        constraintsParens $
        mapM_ (hCommaSep . fmap prettyPrint . unLoc) hst_ctxt -- TODO: Handle comments
      vCon = do
        string constraintsParensL
        space
        forM_ hst_ctxt $ \(L l cs) -> do
          printCommentsBefore l
          inter (newline >> string ", ") $ fmap prettyPrint cs
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
  prettyPrint' x@HsTyVar {} = output x
  prettyPrint' (HsAppTy _ l r) = do
    prettyPrint l
    space
    prettyPrint r
  prettyPrint' HsAppKindTy {} = undefined
  prettyPrint' (HsFunTy _ _ a b) = do
    isVertical <- gets ((InsideVerticalFunctionSignature `elem`) . psInside)
    if isVertical
      then vertical
      else horizontal <-|> vertical
    where
      horizontal = do
        prettyPrint a
        string " -> "
        prettyPrint b
      vertical = do
        exitVerticalFunctionSignature $ prettyPrint a
        newline
        indentedWithSpace (-3) $ string "-> "
        prettyPrint b
  prettyPrint' (HsListTy _ xs) = brackets $ prettyPrint xs
  prettyPrint' (HsTupleTy _ _ xs) = tuple' $ fmap prettyPrint xs
  prettyPrint' HsSumTy {} = undefined
  -- For `HsOpTy`, we do not need a single quote for the infix operator. An
  -- explicit promotion is necessary if there is a data constructor and
  -- a type with the same name. However, infix data constructors never
  -- share their names with types because types cannot contain symbols.
  -- Thus there is no ambiguity.
  prettyPrint' (HsOpTy _ l op r) =
    spaced [prettyPrint l, infixOp $ unLoc op, prettyPrint r]
  prettyPrint' (HsParTy _ inside) = parens $ prettyPrint inside
  prettyPrint' t@HsIParamTy {} = output t
  prettyPrint' HsStarTy {} = undefined
  prettyPrint' HsKindSig {} = undefined
  prettyPrint' (HsSpliceTy _ sp) = prettyPrint sp
  prettyPrint' HsDocTy {} = undefined
  prettyPrint' e@HsBangTy {} = output e
  prettyPrint' HsRecTy {} = undefined
  prettyPrint' (HsExplicitListTy _ _ xs) =
    case xs of
      [] -> string "'[]"
      _  -> hPromotedList $ fmap prettyPrint xs
  prettyPrint' (HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap prettyPrint xs
  prettyPrint' (HsTyLit _ x) = output x
  prettyPrint' HsWildCardTy {} = undefined
  prettyPrint' XHsType {} = undefined

instance PrettyPrintable (HsConDeclGADTDetails GhcPs) where
  prettyPrint' (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> output x
  prettyPrint' (RecConGADT xs) =
    vFields' $
    flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
      output $ head cd_fld_names
      string " :: "
      output cd_fld_type

instance PrettyPrintable (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint' GRHSs {..} = do
    mapM_ prettyPrint grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedBlock $ do
          newline
          isCase <- gets ((InsideCase `elem`) . psInside)
          if isCase
            then indentedDependingOnHead (string "where ") $ do
                   printCommentsBefore epa
                   exitCase $ prettyPrint lr
            else do
              string "where"
              newline
              printCommentsBefore epa
              indentedBlock $ prettyPrint lr
          printCommentsSameLine epa
          printCommentsAfter epa
      _ -> return ()

instance PrettyPrintable (HsMatchContext GhcPs) where
  prettyPrint' FunRhs {..} = prettyPrint mc_fun
  prettyPrint' CaseAlt     = return ()
  prettyPrint' LambdaExpr  = return ()
  prettyPrint' x           = output x

instance PrettyPrintable (ParStmtBlock GhcPs GhcPs) where
  prettyPrint' (ParStmtBlock _ xs _ _) = do
    inVertical <- gets ((InsideVerticalList `elem`) . psInside)
    if inVertical
      then vCommaSep $ fmap prettyPrint xs
      else commaSep $ fmap prettyPrint xs

instance PrettyPrintable RdrName where
  prettyPrint' = prefixOp

instance PrettyPrintable (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint' (GRHS _ [] (L _ (HsDo _ (DoExpr _) body))) = do
    unlessInsideLambda space
    rhsSeparator
    space
    string "do"
    newline
    exitLambda $
      indentedBlock $ do
        printCommentsBefore $ getLoc body
        lined $ prettyPrint <$> unLoc body
        printCommentsSameLine $ getLoc body
        printCommentsAfter $ getLoc body
  prettyPrint' (GRHS _ guards (L _ (HsDo _ (DoExpr _) body))) = do
    isInsideMultiwayIf <- gets ((InsideMultiwayIf `elem`) . psInside)
    unless isInsideMultiwayIf newline
    indentedBlock $ do
      string "| "
      inter
        (if isInsideMultiwayIf
           then comma >> newline
           else newline >> string ", ") $
        fmap prettyPrint guards
      space
      rhsSeparator
      string " do "
      mapM_ prettyPrint $ unLoc body
  prettyPrint' (GRHS _ [] body) = horizontal <-|> vertical
    where
      horizontal = do
        unlessInsideLambda space
        rhsSeparator
        space
        prettyPrint body
      vertical = do
        unlessInsideLambda space
        rhsSeparator
        newline
        exitLambda $ indentedBlock $ prettyPrint body
  prettyPrint' (GRHS _ guards body) = do
    isInsideMultiwayIf <- gets ((InsideMultiwayIf `elem`) . psInside)
    unless isInsideMultiwayIf newline
    (if isInsideMultiwayIf
       then indentedDependingOnHead (string "| ")
       else indentedBlock . (string "| " >>)) $ do
      inter
        (if isInsideMultiwayIf
           then comma >> newline
           else newline >> string ", ") $
        fmap prettyPrint guards
      horizontal <-|> vertical
    where
      horizontal = spacePrefixed [rhsSeparator, prettyPrint body]
      vertical = do
        isInsideMultiwayIf <- gets ((InsideMultiwayIf `elem`) . psInside)
        space
        rhsSeparator
        newline
        (if isInsideMultiwayIf
           then id
           else indentedBlock) $
          prettyPrint body
  commentsBefore (GRHS x _ _) = commentsBefore x
  commentOnSameLine (GRHS x _ _) = commentOnSameLine x
  commentsAfter (GRHS x _ _) = commentsAfter x

instance PrettyPrintable EpaCommentTok where
  prettyPrint' (EpaLineComment c)  = string c
  prettyPrint' (EpaBlockComment c) = string c
  prettyPrint' _                   = undefined

instance PrettyPrintable (SpliceDecl GhcPs) where
  prettyPrint' (SpliceDecl _ sp _) = prettyPrint sp

instance PrettyPrintable (HsSplice GhcPs) where
  prettyPrint' HsTypedSplice {} = undefined
  prettyPrint' (HsUntypedSplice _ decoration _ body) = do
    string prefix
    prettyPrint body
    where
      prefix =
        case decoration of
          DollarSplice -> "$"
          BareSplice   -> ""
  prettyPrint' p@HsQuasiQuote {} = output p
  prettyPrint' HsSpliced {} = undefined

instance PrettyPrintable (Pat GhcPs) where
  prettyPrint' p@WildPat {} = output p
  prettyPrint' p@VarPat {} = output p
  prettyPrint' p@LazyPat {} = output p
  prettyPrint' (AsPat _ a b) = do
    prettyPrint a
    string "@"
    prettyPrint b
  prettyPrint' (ParPat _ inner) = parens $ prettyPrint inner
  prettyPrint' p@BangPat {} = output p
  prettyPrint' (ListPat _ xs) = hList $ fmap prettyPrint xs
  prettyPrint' (TuplePat _ pats _) = hTuple $ fmap prettyPrint pats
  prettyPrint' SumPat {} = undefined
  prettyPrint' ConPat {..} =
    case pat_args of
      PrefixCon _ as -> do
        prefixOp $ unLoc pat_con
        spacePrefixed $ fmap prettyPrint as
      RecCon rec ->
        indentedDependingOnHead (prettyPrint pat_con >> space) $ prettyPrint rec
      InfixCon a b -> do
        prettyPrint a
        unlessSpecialOp (unLoc pat_con) space
        infixOp $ unLoc pat_con
        unlessSpecialOp (unLoc pat_con) space
        prettyPrint b
  prettyPrint' (ViewPat _ l r) = do
    prettyPrint l
    string " -> "
    prettyPrint r
  prettyPrint' p@SplicePat {} = output p
  prettyPrint' p@LitPat {} = output p
  prettyPrint' (NPat _ x _ _) = output x
  prettyPrint' p@NPlusKPat {} = output p
  prettyPrint' p@SigPat {} = output p

instance PrettyPrintable (HsBracket GhcPs) where
  prettyPrint' (ExpBr _ expr) = brackets $ wrapWithBars $ prettyPrint expr
  prettyPrint' (PatBr _ expr) =
    brackets $ do
      string "p"
      wrapWithBars $ prettyPrint expr
  prettyPrint' DecBrL {} = undefined
  prettyPrint' DecBrG {} = undefined
  prettyPrint' (TypBr _ expr) =
    brackets $ do
      string "t"
      wrapWithBars $ prettyPrint expr
  prettyPrint' (VarBr _ True var) = do
    string "'"
    prettyPrint var
  prettyPrint' (VarBr _ False var) = do
    string "''"
    prettyPrint var
  prettyPrint' TExpBr {} = undefined

instance PrettyPrintable SigMethodsFamily where
  prettyPrint' (Sig x)        = prettyPrint x
  prettyPrint' (Method x)     = prettyPrint x
  prettyPrint' (TypeFamily x) = prettyPrint x

instance PrettyPrintable EpaComment where
  prettyPrint' EpaComment {..} = prettyPrint ac_tok

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrint-print 'Anchor'.
instance PrettyPrintable Anchor where
  prettyPrint' _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrint-print 'SrcAnn'.
instance PrettyPrintable (SrcAnn a) where
  prettyPrint' _ = return ()
  commentsBefore (SrcSpanAnn ep _) = commentsBefore ep
  commentOnSameLine (SrcSpanAnn ep _) = commentOnSameLine ep
  commentsAfter (SrcSpanAnn ep _) = commentsAfter ep

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrint-print 'SrcSpan'.
instance PrettyPrintable SrcSpan where
  prettyPrint' _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrint-print 'EpAnn'.
instance PrettyPrintable (EpAnn a) where
  prettyPrint' _ = return ()
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

instance PrettyPrintable (HsLocalBindsLR GhcPs GhcPs) where
  prettyPrint' (HsValBinds _ lr) = prettyPrint lr
  prettyPrint' x                 = output x

instance PrettyPrintable (HsValBindsLR GhcPs GhcPs) where
  prettyPrint' (ValBinds _ methods sigs) =
    lined $ fmap prettyPrint sigsAndMethods
      -- TODO: Merge this where clause with the one in the 'ClassDecl' of
      -- 'TyClDecl'.
    where
      sigsAndMethods =
        sortByLocation $ fmap Sig sigs ++ fmap Method (bagToList methods)
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Sig x)       = realSrcSpan $ locA $ getLoc x
      getLocation (Method x)    = realSrcSpan $ locA $ getLoc x
      getLocation TypeFamily {} = undefined
  prettyPrint' x = output x

instance PrettyPrintable (HsTupArg GhcPs) where
  prettyPrint' (Present _ e) = prettyPrint e
  prettyPrint' Missing {}    = return () -- This appears in a tuple section.

-- FIXME: Reconsider using a type variable. Using type variables may need
-- to define odd instances (e.g., Void).
instance (PrettyPrintable a, PrettyPrintable b) =>
         PrettyPrintable (HsRecField' a b) where
  prettyPrint' HsRecField {..} = horizontal <-|> vertical
    where
      horizontal = do
        prettyPrint hsRecFieldLbl
        unless hsRecPun $ do
          string " = "
          prettyPrint hsRecFieldArg
      vertical = do
        prettyPrint hsRecFieldLbl
        unless hsRecPun $ do
          string " ="
          newline
          indentedBlock $ prettyPrint hsRecFieldArg

instance PrettyPrintable (FieldOcc GhcPs) where
  prettyPrint' FieldOcc {..} = prettyPrint rdrNameFieldOcc

-- HsConDeclH98Details
instance PrettyPrintable (HsConDetails Void (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs))) (GenLocated SrcSpanAnnL [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  prettyPrint' (PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap prettyPrint xs
      vertical = indentedBlock $ newlinePrefixed $ fmap prettyPrint xs
  prettyPrint' (RecCon (L _ rec)) = do
    newline
    indentedBlock $ vFields $ fmap prettyPrint rec
  prettyPrint' InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."

-- FIXME: Reconsider using a type variable.
instance PrettyPrintable a => PrettyPrintable (HsScaled GhcPs a) where
  prettyPrint' (HsScaled _ x) = prettyPrint x

instance PrettyPrintable (ConDeclField GhcPs) where
  prettyPrint' ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    prettyPrint $ head cd_fld_names
    string " :: "
    prettyPrint cd_fld_type

instance PrettyPrintable InfixExpr where
  prettyPrint' (InfixExpr (L _ (HsVar _ bind))) = infixOp $ unLoc bind
  prettyPrint' (InfixExpr x)                    = prettyPrint' x
  commentsBefore (InfixExpr x) = commentsBefore x
  commentOnSameLine (InfixExpr x) = commentOnSameLine x
  commentsAfter (InfixExpr x) = commentsAfter x

instance PrettyPrintable InfixApp where
  prettyPrint' InfixApp {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced [prettyPrint lhs, prettyPrint (InfixExpr op), prettyPrint rhs]
      vertical = do
        lhsVer
        beforeRhs <-
          case unLoc lhs of
            (HsDo _ DoExpr {} _) -> do
              indentedWithSpace 3 (newline >> prettyPrint (InfixExpr op)) -- 3 for "do "
              return space
            _ -> do
              space
              prettyPrint (InfixExpr op)
              return newline
        (if immediatelyAfterDo
           then indentedBlock
           else id) $
          case unLoc rhs of
            (HsDo _ (DoExpr _) xs) -> do
              string " do"
              newline
              indentedBlock $ lined $ prettyPrint <$> unLoc xs -- TODO: Handle comments.
            HsLam {} -> do
              space
              prettyPrint rhs
            HsLamCase {} -> do
              space
              prettyPrint rhs
            _ -> do
              beforeRhs
              col <- startingColumn
              (if col == 0
                 then indentedBlock
                 else id) $
                prettyPrint rhs
      lhsVer =
        case lhs of
          (L loc (OpApp _ l o r)) ->
            prettyPrint (L loc (InfixApp l o r immediatelyAfterDo))
          _ -> prettyPrint lhs

instance PrettyPrintable a => PrettyPrintable (BooleanFormula a) where
  prettyPrint' (Var x)    = prettyPrint x
  prettyPrint' (And xs)   = commaSep $ fmap prettyPrint xs
  prettyPrint' (Or xs)    = barSep $ fmap prettyPrint xs
  prettyPrint' (Parens x) = parens $ prettyPrint x

instance PrettyPrintable (FieldLabelStrings GhcPs) where
  prettyPrint' _ = undefined

instance PrettyPrintable (AmbiguousFieldOcc GhcPs) where
  prettyPrint' (Unambiguous _ name) = prettyPrint name
  prettyPrint' (Ambiguous _ name)   = prettyPrint name

instance PrettyPrintable (ImportDecl GhcPs) where
  prettyPrint' ImportDecl {..} = do
    string "import "
    when (ideclSource == IsBoot) $ string "{-# SOURCE #-} "
    when ideclSafe $ string "safe "
    unless (ideclQualified == NotQualified) $ string "qualified "
    whenJust ideclPkgQual $ \x -> do
      prettyPrint x
      space
    output ideclName
    whenJust ideclAs $ \x -> do
      string " as "
      output x
    whenJust ideclHiding $ \(x, _) -> do
      when x (string " hiding")
      (string " " >> hTuple explicitOrHidingImports) <-|>
        (newline >> indentedBlock (vTuple explicitOrHidingImports))
    where
      explicitOrHidingImports =
        output <$> maybe [] (fmap unLoc . unLoc . snd) ideclHiding

instance PrettyPrintable (HsDerivingClause GhcPs) where
  prettyPrint' HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      output x
      space
    prettyPrint deriv_clause_tys

instance PrettyPrintable (DerivClauseTys GhcPs) where
  prettyPrint' (DctSingle _ ty) = parens $ prettyPrint ty
  prettyPrint' (DctMulti _ ts)  = tuple $ fmap prettyPrint ts

instance PrettyPrintable OverlapMode where
  prettyPrint' NoOverlap {}    = undefined
  prettyPrint' Overlappable {} = undefined
  prettyPrint' Overlapping {}  = string "{-# OVERLAPPING #-}"
  prettyPrint' Overlaps {}     = undefined
  prettyPrint' Incoherent {}   = undefined

instance PrettyPrintable StringLiteral where
  prettyPrint' = output

instance PrettyPrintable (FamilyDecl GhcPs) where
  prettyPrint' FamilyDecl {..} = do
    string "type "
    prettyPrint fdLName
    spacePrefixed $ prettyPrint <$> hsq_explicit fdTyVars
    string " = "
    prettyPrint fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      prettyPrint x

instance PrettyPrintable (FamilyResultSig GhcPs) where
  prettyPrint' NoSig {}       = undefined
  prettyPrint' KindSig {}     = undefined
  prettyPrint' (TyVarSig _ x) = prettyPrint x

instance PrettyPrintable (HsTyVarBndr a GhcPs) where
  prettyPrint' (UserTyVar _ _ x) = prettyPrint x
  prettyPrint' KindedTyVar {}    = undefined

instance PrettyPrintable (InjectivityAnn GhcPs) where
  prettyPrint' (InjectivityAnn _ from to) = do
    prettyPrint from
    string " -> "
    spaced $ fmap prettyPrint to

instance PrettyPrintable (ArithSeqInfo GhcPs) where
  prettyPrint' (From from) =
    brackets $ do
      prettyPrint from
      string " .."
  prettyPrint' FromThen {} = undefined
  prettyPrint' FromTo {} = undefined
  prettyPrint' FromThenTo {} = undefined

prefixExpr :: HsExpr GhcPs -> Printer ()
prefixExpr (HsVar _ bind) = prefixOp $ unLoc bind
prefixExpr x              = prettyPrint x
