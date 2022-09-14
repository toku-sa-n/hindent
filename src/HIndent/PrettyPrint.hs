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
  ( prettyPrintWithComments
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

-- | This function prettyPrintWithComments-prints the given AST node with comments.
prettyPrintWithComments :: PrettyPrintable a => a -> Printer ()
prettyPrintWithComments p = do
  printCommentsBefore p
  prettyPrint p
  printCommentsSameLine p
  printCommentsAfter p

printCommentsBefore :: PrettyPrintable a => a -> Printer ()
printCommentsBefore p =
  forM_ (commentsBefore p) $ \x -> do
    prettyPrintWithComments x
    newline

printCommentsSameLine :: PrettyPrintable a => a -> Printer ()
printCommentsSameLine (commentOnSameLine -> Just (L sp c)) = do
  col <- gets psColumn
  if col == 0
    then indentedWithLevel (fromIntegral $ srcSpanStartCol $ anchor sp) $
         prettyPrintWithComments c
    else do
      space
      prettyPrintWithComments c
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
        indentedWithLevel col $ prettyPrintWithComments c
        eolCommentsArePrinted

-- | Pretty print including comments.
--
-- FIXME: 'Pretty' has a problem. It has two responsibilities; one is to
-- print a given node prettyPrintWithComments, and the other is to collect comments from the
-- node.
--
-- Note that there are three types of nodes:
-- * A node that can prettyPrintWithComments-print and has comments (e.g., 'HsModule')
-- * A node that can prettyPrintWithComments-print but has no comments (e.g., almost all
-- nodes)
-- * A node that cannot prettyPrintWithComments-print but has comments (e.g., 'EpAnn')
class PrettyPrintable a where
  prettyPrint :: a -> Printer ()
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
  prettyPrint m = blanklined printers
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
        mapM_ (\(x, sp) -> prettyPrintWithComments x >> fromMaybe (return ()) sp) $
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
      outputImportGroup = lined . fmap prettyPrintWithComments
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
-- (e.g., 'EpAnn') cannot prettyPrintWithComments-print. The restriction exists only for
-- extracting comments. Remove the restriction.
instance (PrettyPrintable l, PrettyPrintable e) =>
         PrettyPrintable (GenLocated l e) where
  prettyPrint (L _ e) = prettyPrintWithComments e
  commentsBefore (L l _) = commentsBefore l
  commentOnSameLine (L l _) = commentOnSameLine l
  commentsAfter (L l _) = commentsAfter l

instance PrettyPrintable (HsDecl GhcPs) where
  prettyPrint (TyClD _ d)    = prettyPrintWithComments d
  prettyPrint (InstD _ inst) = prettyPrintWithComments inst
  prettyPrint DerivD {}      = undefined
  prettyPrint (ValD _ bind)  = prettyPrintWithComments bind
  prettyPrint (SigD _ s)     = insideSignature $ prettyPrintWithComments s
  prettyPrint KindSigD {}    = undefined
  prettyPrint DefD {}        = undefined
  prettyPrint x@ForD {}      = output x
  prettyPrint WarningD {}    = undefined
  prettyPrint AnnD {}        = undefined
  prettyPrint RuleD {}       = undefined
  prettyPrint (SpliceD _ sp) = prettyPrintWithComments sp
  prettyPrint DocD {}        = return ()
  prettyPrint RoleAnnotD {}  = undefined

instance PrettyPrintable (TyClDecl GhcPs) where
  prettyPrint SynDecl {..} = do
    string "type "
    -- TODO: Merge this case with the one in 'ClassDecl's branch.
    case tcdFixity of
      Prefix ->
        spaced $ prettyPrintWithComments tcdLName : fmap output (hsq_explicit tcdTyVars)
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
        prettyPrintWithComments tcdRhs
      ver =
        indentedWithSpace 3 $ do
          newline
          string "= "
          indentedBlock $ prettyPrintWithComments tcdRhs
  prettyPrint DataDecl {..} = do
    case dd_ND tcdDataDefn of
      DataType -> string "data "
      NewType  -> string "newtype "
    output tcdLName
    forM_ (hsq_explicit tcdTyVars) $ \x -> do
      space
      output x
    prettyPrintWithComments tcdDataDefn
  prettyPrint ClassDecl {..} = do
    if isJust tcdCtxt
      then verHead
      else horHead <-|> verHead
    newline
    indentedBlock $ lined $ fmap prettyPrintWithComments sigsMethodsFamilies
    where
      horHead = do
        string "class "
        case tcdFixity of
          Prefix ->
            spaced $ prettyPrintWithComments tcdLName : fmap output (hsq_explicit tcdTyVars)
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
            spaced $ fmap prettyPrintWithComments from
            string " -> "
            spaced $ fmap prettyPrintWithComments to
        unless (null sigsMethodsFamilies) $ string " where"
      verHead = do
        indentedDependingOnHead (string "class ") $ do
          whenJust tcdCtxt $ \(L _ xs) ->
            case xs -- TODO: Handle comments.
                  of
              [] -> undefined
              [x] -> do
                prettyPrintWithComments x
                string " =>"
                newline
              _ -> do
                hTuple $ fmap prettyPrintWithComments xs
                string " =>"
                newline
          case tcdFixity of
            Prefix ->
              spaced $
              prettyPrintWithComments tcdLName : fmap output (hsq_explicit tcdTyVars)
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
              spaced $ fmap prettyPrintWithComments from
              string " -> "
              spaced $ fmap prettyPrintWithComments to
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
  prettyPrint x = output x

instance PrettyPrintable (InstDecl GhcPs) where
  prettyPrint ClsInstD {..} = prettyPrintWithComments cid_inst
  prettyPrint x             = output x

instance PrettyPrintable (HsBind GhcPs) where
  prettyPrint FunBind {..} = prettyPrintWithComments fun_matches
  prettyPrint x            = output x
  commentsBefore FunBind {..} = commentsBefore fun_id
  commentsBefore _            = []
  commentOnSameLine FunBind {..} = commentOnSameLine fun_id
  commentOnSameLine _            = Nothing
  commentsAfter FunBind {..} = commentsAfter fun_id
  commentsAfter _            = []

instance PrettyPrintable (Sig GhcPs) where
  prettyPrint (TypeSig _ funName params) = do
    prettyPrintWithComments $ head funName
    horizontal <-|> vertical
    where
      horizontal = do
        string " :: "
        prettyPrintWithComments $ hswc_body params
      vertical =
        insideVerticalFunctionSignature $ do
          if isUsingForall
            then string " :: "
            else do
              string " ::"
              newline
          indentedBlock $ indentedWithSpace 3 $ prettyPrintWithComments $ hswc_body params -- 3 for "-> "
      isUsingForall =
        case sig_bndrs (unLoc $ hswc_body params) of
          HsOuterExplicit {} -> True
          _                  -> False
  prettyPrint (ClassOpSig _ isDefault funNames params) = do
    when isDefault $ string "default "
    hCommaSep $ fmap prettyPrintWithComments funNames
    string " :: "
    prettyPrintWithComments $ sig_body $ unLoc params
  prettyPrint (MinimalSig _ _ xs) =
    indentedDependingOnHead (string "{-# MINIMAL ") $ do
      prettyPrintWithComments xs
      string " #-}"
  prettyPrint x = output x

instance PrettyPrintable (HsDataDefn GhcPs) where
  prettyPrint HsDataDefn {..} =
    case dd_kindSig of
      Just kindSig -> do
        string " :: "
        output kindSig
        string " where"
        indentedBlock $ newlinePrefixed $ fmap prettyPrintWithComments dd_cons
      Nothing ->
        indentedBlock $ do
          case length dd_cons of
            0 -> pure ()
            1 -> do
              string " ="
              newline
              prettyPrintWithComments $ head dd_cons
            _ -> do
              newline
              indentedDependingOnHead (string "= ") $
                vBarSep $ fmap prettyPrintWithComments dd_cons
          unless (null dd_derivs) $ do
            newline
            lined $ fmap prettyPrintWithComments dd_derivs

instance PrettyPrintable (ClsInstDecl GhcPs) where
  prettyPrint ClsInstDecl {..} = do
    indentedDependingOnHead (string "instance ") $
      insideInstDecl $ do
        whenJust cid_overlap_mode $ \x -> do
          prettyPrintWithComments x
          space
        prettyPrintWithComments cid_poly_ty
    unless (isEmptyBag cid_binds) $ do
      string " where"
      newline
      indentedBlock $ mapM_ prettyPrintWithComments cid_binds

instance PrettyPrintable (MatchGroup GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint MG {..} = lined $ prettyPrintWithComments <$> unLoc mg_alts

instance PrettyPrintable (HsExpr GhcPs) where
  prettyPrint v@HsVar {} = prefixExpr v
  prettyPrint HsUnboundVar {} = undefined
  prettyPrint HsConLikeOut {} = undefined
  prettyPrint HsRecFld {} = undefined
  prettyPrint (HsOverLabel _ l) = do
    string "#"
    output l
  prettyPrint HsIPVar {} = undefined
  prettyPrint full@HsOverLit {} = output full
  prettyPrint (HsLit _ l) = output l
  prettyPrint (HsLam _ body) = insideLambda $ prettyPrintWithComments body
  prettyPrint (HsLamCase _ matches) =
    insideCase $ do
      string "\\case"
      if null $ unLoc $ mg_alts matches
        then string " {}"
        else do
          newline
          indentedBlock $ prettyPrintWithComments matches
  prettyPrint (HsApp _ l r) = horizontal <-|> vertical
    where
      horizontal = spaced [prettyPrintWithComments l, prettyPrintWithComments r]
      vertical = do
        let (f, args) =
              case flatten l ++ [r] of
                (f':args') -> (f', args')
                _          -> error "Invalid function application."
        col <- gets psColumn
        spaces <- getIndentSpaces
        prettyPrintWithComments f
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
        indentedWithSpace spaces' $ lined $ fmap prettyPrintWithComments args
      flatten :: LHsExpr GhcPs -> [LHsExpr GhcPs]
      flatten (L (SrcSpanAnn (EpAnn _ _ cs) _) (HsApp _ l' r')) =
        flatten l' ++ [insertComments cs r']
      flatten x = [x]
      insertComments :: EpAnnComments -> LHsExpr GhcPs -> LHsExpr GhcPs
      insertComments cs (L s@SrcSpanAnn {ann = e@EpAnn {comments = cs'}} r') =
        L (s {ann = e {comments = cs <> cs'}}) r'
      insertComments _ x = x
  prettyPrint t@HsAppType {} = output t
  prettyPrint (OpApp _ l o r) = prettyPrintWithComments (InfixApp l o r False)
  prettyPrint NegApp {} = undefined
  prettyPrint (HsPar _ expr) = parens $ prettyPrintWithComments expr
  prettyPrint (SectionL _ l o) =
    spaced [prettyPrintWithComments l, prettyPrintWithComments (InfixExpr o)]
  prettyPrint (SectionR _ o r) =
    spaced [prettyPrintWithComments (InfixExpr o), prettyPrintWithComments r]
  prettyPrint (ExplicitTuple _ full _) = horizontal <-|> vertical
    where
      horizontal = hTuple $ fmap prettyPrintWithComments full
      vertical =
        parens $
        prefixedLined "," $
        fmap
          (\e ->
             unless
               (isMissing e)
               (indentedDependingOnHead space $ prettyPrintWithComments e))
          full
      isMissing Missing {} = True
      isMissing _          = False
  prettyPrint ExplicitSum {} = undefined
  prettyPrint (HsCase _ cond arms) =
    insideCase $ do
      indentedDependingOnHead (string "case ") $ do
        prettyPrintWithComments cond
        string " of"
      if null $ unLoc $ mg_alts arms
        then string " {}"
        else do
          newline
          indentedBlock $ prettyPrintWithComments arms
  prettyPrint (HsIf _ cond t f) = do
    string "if "
    prettyPrintWithComments cond
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
            indentedBlock $ lined $ prettyPrintWithComments <$> unLoc xs -- TODO: Handle comments.
          _ -> indentedDependingOnHead (string str) $ prettyPrintWithComments e
  prettyPrint (HsMultiIf _ guards) =
    indentedDependingOnHead (string "if ") $
    insideMultiwayIf $ lined $ fmap prettyPrintWithComments guards
  prettyPrint (HsLet _ binds exprs) = do
    indentedDependingOnHead (string "let ") $ prettyPrintWithComments binds
    newline
    indentedDependingOnHead (string " in ") $ prettyPrintWithComments exprs
  prettyPrint (HsDo _ (DoExpr _) xs) =
    indentedDependingOnHead (string "do ") $ lined $ output <$> unLoc xs -- TODO: Handle comments.
  -- While the name contains "Monad", this branch seems to be for list comprehensions.
  prettyPrint (HsDo _ MonadComp xs) = horizontal <-|> vertical
    where
      horizontal =
        brackets $ do
          prettyPrintWithComments $ head $ unLoc xs
          string " | "
          hCommaSep $ fmap prettyPrintWithComments $ tail $ unLoc xs -- TODO: Handle comments.
      vertical =
        insideVerticalList $
        if null $ unLoc xs
          then string "[]"
          else let (lastStmt, others) = (head $ unLoc xs, tail $ unLoc xs)
                in do string "[ "
                      prettyPrintWithComments lastStmt
                      newline
                      forM_ (stmtsAndPrefixes others) $ \(p, x) -> do
                        indentedDependingOnHead (string p) $ prettyPrintWithComments x
                        newline
                      string "]"
      stmtsAndPrefixes l = ("| ", head l) : fmap (", ", ) (tail l)
  prettyPrint HsDo {} = undefined
  prettyPrint (ExplicitList _ xs) = horizontal <-|> vertical
    where
      horizontal = brackets $ hCommaSep $ fmap prettyPrintWithComments xs
      vertical = vList $ fmap prettyPrintWithComments xs
  prettyPrint (RecordCon _ name fields) = horizontal <-|> vertical
    where
      horizontal = do
        name'
        space
        prettyPrintWithComments fields
      vertical = do
        name'
        (space >> prettyPrintWithComments fields) <-|>
          (newline >> indentedBlock (prettyPrintWithComments fields))
      name' =
        if head (showOutputable name) == ':'
          then parens $ output name
          else output name
  prettyPrint (RecordUpd _ name fields) = do
    prettyPrintWithComments name
    space
    braces $
      -- TODO: Refactor this case.
      case fields of
        Right xs ->
          forM_ xs $ \(L l HsRecField {..}) -> do
            printCommentsBefore l
            prettyPrintWithComments hsRecFieldLbl
            string " = "
            prettyPrintWithComments hsRecFieldArg
            printCommentsSameLine l
            printCommentsAfter l
        Left xs ->
          forM_ xs $ \(L l HsRecField {..}) -> do
            printCommentsBefore l
            prettyPrintWithComments hsRecFieldLbl
            string " = "
            prettyPrintWithComments hsRecFieldArg
            printCommentsSameLine l
            printCommentsAfter l
  prettyPrint HsGetField {} = undefined
  prettyPrint HsProjection {} = undefined
  prettyPrint (ExprWithTySig _ e sig) = do
    prettyPrintWithComments e
    string " :: "
    prettyPrintWithComments $ hswc_body sig
  prettyPrint (ArithSeq _ _ x) = prettyPrintWithComments x
  prettyPrint (HsBracket _ inner) = prettyPrintWithComments inner
  prettyPrint HsRnBracketOut {} = undefined
  prettyPrint HsTcBracketOut {} = undefined
  prettyPrint (HsSpliceE _ x) = prettyPrintWithComments x
  prettyPrint HsProc {} = undefined
  prettyPrint HsStatic {} = undefined
  prettyPrint HsTick {} = undefined
  prettyPrint HsBinTick {} = undefined
  prettyPrint HsPragE {} = undefined
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
  prettyPrint HsSig {..} = do
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
    prettyPrintWithComments sig_body

instance PrettyPrintable (ConDecl GhcPs) where
  prettyPrint ConDeclGADT {..} = horizontal <-|> vertical
    where
      horizontal = do
        output $ head con_names
        string " :: "
        prettyPrintWithComments con_g_args
        string " -> "
        output con_res_ty
      vertical = do
        output $ head con_names
        newline
        indentedBlock $ do
          indentedDependingOnHead (string ":: ") $ prettyPrintWithComments con_g_args
          newline
          string "-> "
          output con_res_ty
  prettyPrint ConDeclH98 {..}
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
                     prettyPrintWithComments x
                     string " =>"
                     newline
                   Just (L _ xs) -> do
                     hTuple $ fmap prettyPrintWithComments xs
                     string " =>"
                     newline
                 prettyPrintWithComments con_name
                 prettyPrintWithComments con_args)
      else do
        case con_args of
          (InfixCon l r) ->
            spaced [prettyPrintWithComments l, infixOp $ unLoc con_name, prettyPrintWithComments r] -- TODO: Handle comments.
          _ -> do
            prettyPrintWithComments con_name
            prettyPrintWithComments con_args

instance PrettyPrintable (Match GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint Match {..} = do
    isInsideCase <- gets ((InsideCase `elem`) . psInside)
    isInsideLambda <- gets ((InsideLambda `elem`) . psInside)
    whenInsideLambda $ string "\\"
    case (isInsideCase, isInsideLambda, mc_fixity m_ctxt) of
      (True, _, _) -> do
        mapM_ prettyPrintWithComments m_pats
        prettyPrintWithComments m_grhss
      (_, True, _) -> do
        unless (null m_pats) $
          case unLoc $ head m_pats of
            LazyPat {} -> space
            BangPat {} -> space
            _          -> return ()
        spaced $ fmap prettyPrintWithComments m_pats ++ [prettyPrintWithComments m_grhss]
      (_, _, Prefix) -> do
        prettyPrintWithComments m_ctxt
        unless (null m_pats) $ do
          space
          spaced $ fmap prettyPrintWithComments m_pats
        prettyPrintWithComments m_grhss
      (_, _, Infix) -> do
        case (m_pats, m_ctxt) of
          (l:r:xs, FunRhs {..}) -> do
            spaced $
              [prettyPrintWithComments l, infixOp $ unLoc mc_fun, prettyPrintWithComments r] ++
              fmap prettyPrintWithComments xs
            prettyPrintWithComments m_grhss
          _ -> error "Not enough parameters are passed."
  commentsBefore Match {..} = commentsBefore m_ext
  commentOnSameLine Match {..} = commentOnSameLine m_ext
  commentsAfter Match {..} = commentsAfter m_ext

instance PrettyPrintable (StmtLR GhcPs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint l@LastStmt {} = output l
  prettyPrint full@(BindStmt _ pat body) =
    output full <-|> do
      output pat
      string " <-"
      newline
      indentedBlock $ prettyPrintWithComments body
  prettyPrint ApplicativeStmt {} = undefined
  prettyPrint (BodyStmt _ (L loc (OpApp _ l o r)) _ _) =
    prettyPrintWithComments (L loc (InfixApp l o r True))
  prettyPrint (BodyStmt _ body _ _) = prettyPrintWithComments body
  prettyPrint (LetStmt _ l) = do
    string "let "
    prettyPrintWithComments l
  prettyPrint (ParStmt _ xs _ _) = do
    inVertical <- gets ((InsideVerticalList `elem`) . psInside)
    if inVertical
      then vertical
      else horizontal <-|> vertical
    where
      horizontal = hBarSep $ fmap output xs
      vertical = vBarSep $ fmap prettyPrintWithComments xs
  prettyPrint TransStmt {..} =
    vCommaSep $
    fmap prettyPrintWithComments trS_stmts ++ [string "then " >> prettyPrintWithComments trS_using]
  prettyPrint RecStmt {} = undefined
  commentsBefore (LetStmt l _) = commentsBefore l
  commentsBefore _             = []
  commentsAfter (LetStmt l _) = commentsAfter l
  commentsAfter _             = []

-- FIXME: Reconsider using a type variable. Using type variables may need
-- to define odd instances (e.g., Void).
instance PrettyPrintable a => PrettyPrintable (HsRecFields GhcPs a) where
  prettyPrint HsRecFields {..} = horizontal <-|> vertical
    where
      horizontal =
        case rec_dotdot of
          Just _  -> braces $ string ".."
          Nothing -> hFields $ fmap prettyPrintWithComments rec_flds
      vertical = vFields $ fmap prettyPrintWithComments rec_flds

instance PrettyPrintable (HsType GhcPs) where
  prettyPrint HsForAllTy {} = undefined
  prettyPrint HsQualTy {..} = do
    isInSig <- gets ((InsideSignature `elem`) . psInside)
    if isInSig
      then sigHor <-|> sigVer
      else notInSig
    where
      sigHor = do
        constraints
        string " => "
        prettyPrintWithComments hst_body
      sigVer = do
        constraints
        newline
        indentedWithSpace (-3) $ string "=> "
        prettyPrintWithComments hst_body
      notInSig = do
        isInst <- gets ((InsideInstDecl `elem`) . psInside)
        if isInst
          then notHor <-|> notVer
          else notVer
      notHor = do
        constraints
        string " => "
        prettyPrintWithComments hst_body
      notVer = do
        constraints
        string " =>"
        newline
        isInst <- gets ((InsideInstDecl `elem`) . psInside)
        (if isInst
           then id
           else indentedBlock) $
          prettyPrintWithComments hst_body
      constraints = hCon <-|> vCon
      hCon =
        constraintsParens $
        mapM_ (hCommaSep . fmap prettyPrintWithComments . unLoc) hst_ctxt -- TODO: Handle comments
      vCon = do
        string constraintsParensL
        space
        forM_ hst_ctxt $ \(L l cs) -> do
          printCommentsBefore l
          inter (newline >> string ", ") $ fmap prettyPrintWithComments cs
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
  prettyPrint x@HsTyVar {} = output x
  prettyPrint (HsAppTy _ l r) = do
    prettyPrintWithComments l
    space
    prettyPrintWithComments r
  prettyPrint HsAppKindTy {} = undefined
  prettyPrint (HsFunTy _ _ a b) = do
    isVertical <- gets ((InsideVerticalFunctionSignature `elem`) . psInside)
    if isVertical
      then vertical
      else horizontal <-|> vertical
    where
      horizontal = do
        prettyPrintWithComments a
        string " -> "
        prettyPrintWithComments b
      vertical = do
        exitVerticalFunctionSignature $ prettyPrintWithComments a
        newline
        indentedWithSpace (-3) $ string "-> "
        prettyPrintWithComments b
  prettyPrint (HsListTy _ xs) = brackets $ prettyPrintWithComments xs
  prettyPrint (HsTupleTy _ _ xs) = tuple' $ fmap prettyPrintWithComments xs
  prettyPrint HsSumTy {} = undefined
  -- For `HsOpTy`, we do not need a single quote for the infix operator. An
  -- explicit promotion is necessary if there is a data constructor and
  -- a type with the same name. However, infix data constructors never
  -- share their names with types because types cannot contain symbols.
  -- Thus there is no ambiguity.
  prettyPrint (HsOpTy _ l op r) =
    spaced [prettyPrintWithComments l, infixOp $ unLoc op, prettyPrintWithComments r]
  prettyPrint (HsParTy _ inside) = parens $ prettyPrintWithComments inside
  prettyPrint t@HsIParamTy {} = output t
  prettyPrint HsStarTy {} = undefined
  prettyPrint HsKindSig {} = undefined
  prettyPrint (HsSpliceTy _ sp) = prettyPrintWithComments sp
  prettyPrint HsDocTy {} = undefined
  prettyPrint e@HsBangTy {} = output e
  prettyPrint HsRecTy {} = undefined
  prettyPrint (HsExplicitListTy _ _ xs) =
    case xs of
      [] -> string "'[]"
      _  -> hPromotedList $ fmap prettyPrintWithComments xs
  prettyPrint (HsExplicitTupleTy _ xs) = hPromotedTuple $ fmap prettyPrintWithComments xs
  prettyPrint (HsTyLit _ x) = output x
  prettyPrint HsWildCardTy {} = undefined
  prettyPrint XHsType {} = undefined

instance PrettyPrintable (HsConDeclGADTDetails GhcPs) where
  prettyPrint (PrefixConGADT xs) =
    inter (string " -> ") $
    flip fmap xs $ \case
      (HsScaled _ x) -> output x
  prettyPrint (RecConGADT xs) =
    vFields' $
    flip fmap (unLoc xs) $ \(L _ ConDeclField {..}) -> do
      output $ head cd_fld_names
      string " :: "
      output cd_fld_type

instance PrettyPrintable (GRHSs GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint GRHSs {..} = do
    mapM_ prettyPrintWithComments grhssGRHSs
    case grhssLocalBinds of
      (HsValBinds epa lr) ->
        indentedBlock $ do
          newline
          isCase <- gets ((InsideCase `elem`) . psInside)
          if isCase
            then indentedDependingOnHead (string "where ") $ do
                   printCommentsBefore epa
                   exitCase $ prettyPrintWithComments lr
            else do
              string "where"
              newline
              printCommentsBefore epa
              indentedBlock $ prettyPrintWithComments lr
          printCommentsSameLine epa
          printCommentsAfter epa
      _ -> return ()

instance PrettyPrintable (HsMatchContext GhcPs) where
  prettyPrint FunRhs {..} = prettyPrintWithComments mc_fun
  prettyPrint CaseAlt     = return ()
  prettyPrint LambdaExpr  = return ()
  prettyPrint x           = output x

instance PrettyPrintable (ParStmtBlock GhcPs GhcPs) where
  prettyPrint (ParStmtBlock _ xs _ _) = do
    inVertical <- gets ((InsideVerticalList `elem`) . psInside)
    if inVertical
      then vCommaSep $ fmap prettyPrintWithComments xs
      else commaSep $ fmap prettyPrintWithComments xs

instance PrettyPrintable RdrName where
  prettyPrint = prefixOp

instance PrettyPrintable (GRHS GhcPs (GenLocated SrcSpanAnnA (HsExpr GhcPs))) where
  prettyPrint (GRHS _ [] (L _ (HsDo _ (DoExpr _) body))) = do
    unlessInsideLambda space
    rhsSeparator
    space
    string "do"
    newline
    exitLambda $
      indentedBlock $ do
        printCommentsBefore $ getLoc body
        lined $ prettyPrintWithComments <$> unLoc body
        printCommentsSameLine $ getLoc body
        printCommentsAfter $ getLoc body
  prettyPrint (GRHS _ guards (L _ (HsDo _ (DoExpr _) body))) = do
    isInsideMultiwayIf <- gets ((InsideMultiwayIf `elem`) . psInside)
    unless isInsideMultiwayIf newline
    indentedBlock $ do
      string "| "
      inter
        (if isInsideMultiwayIf
           then comma >> newline
           else newline >> string ", ") $
        fmap prettyPrintWithComments guards
      space
      rhsSeparator
      string " do "
      mapM_ prettyPrintWithComments $ unLoc body
  prettyPrint (GRHS _ [] body) = horizontal <-|> vertical
    where
      horizontal = do
        unlessInsideLambda space
        rhsSeparator
        space
        prettyPrintWithComments body
      vertical = do
        unlessInsideLambda space
        rhsSeparator
        newline
        exitLambda $ indentedBlock $ prettyPrintWithComments body
  prettyPrint (GRHS _ guards body) = do
    isInsideMultiwayIf <- gets ((InsideMultiwayIf `elem`) . psInside)
    unless isInsideMultiwayIf newline
    (if isInsideMultiwayIf
       then indentedDependingOnHead (string "| ")
       else indentedBlock . (string "| " >>)) $ do
      inter
        (if isInsideMultiwayIf
           then comma >> newline
           else newline >> string ", ") $
        fmap prettyPrintWithComments guards
      horizontal <-|> vertical
    where
      horizontal = spacePrefixed [rhsSeparator, prettyPrintWithComments body]
      vertical = do
        isInsideMultiwayIf <- gets ((InsideMultiwayIf `elem`) . psInside)
        space
        rhsSeparator
        newline
        (if isInsideMultiwayIf
           then id
           else indentedBlock) $
          prettyPrintWithComments body
  commentsBefore (GRHS x _ _) = commentsBefore x
  commentOnSameLine (GRHS x _ _) = commentOnSameLine x
  commentsAfter (GRHS x _ _) = commentsAfter x

instance PrettyPrintable EpaCommentTok where
  prettyPrint (EpaLineComment c)  = string c
  prettyPrint (EpaBlockComment c) = string c
  prettyPrint _                   = undefined

instance PrettyPrintable (SpliceDecl GhcPs) where
  prettyPrint (SpliceDecl _ sp _) = prettyPrintWithComments sp

instance PrettyPrintable (HsSplice GhcPs) where
  prettyPrint HsTypedSplice {} = undefined
  prettyPrint (HsUntypedSplice _ decoration _ body) = do
    string prefix
    prettyPrintWithComments body
    where
      prefix =
        case decoration of
          DollarSplice -> "$"
          BareSplice   -> ""
  prettyPrint p@HsQuasiQuote {} = output p
  prettyPrint HsSpliced {} = undefined

instance PrettyPrintable (Pat GhcPs) where
  prettyPrint p@WildPat {} = output p
  prettyPrint p@VarPat {} = output p
  prettyPrint p@LazyPat {} = output p
  prettyPrint (AsPat _ a b) = do
    prettyPrintWithComments a
    string "@"
    prettyPrintWithComments b
  prettyPrint (ParPat _ inner) = parens $ prettyPrintWithComments inner
  prettyPrint p@BangPat {} = output p
  prettyPrint (ListPat _ xs) = hList $ fmap prettyPrintWithComments xs
  prettyPrint (TuplePat _ pats _) = hTuple $ fmap prettyPrintWithComments pats
  prettyPrint SumPat {} = undefined
  prettyPrint ConPat {..} =
    case pat_args of
      PrefixCon _ as -> do
        prefixOp $ unLoc pat_con
        spacePrefixed $ fmap prettyPrintWithComments as
      RecCon rec ->
        indentedDependingOnHead (prettyPrintWithComments pat_con >> space) $ prettyPrintWithComments rec
      InfixCon a b -> do
        prettyPrintWithComments a
        unlessSpecialOp (unLoc pat_con) space
        infixOp $ unLoc pat_con
        unlessSpecialOp (unLoc pat_con) space
        prettyPrintWithComments b
  prettyPrint (ViewPat _ l r) = do
    prettyPrintWithComments l
    string " -> "
    prettyPrintWithComments r
  prettyPrint p@SplicePat {} = output p
  prettyPrint p@LitPat {} = output p
  prettyPrint (NPat _ x _ _) = output x
  prettyPrint p@NPlusKPat {} = output p
  prettyPrint p@SigPat {} = output p

instance PrettyPrintable (HsBracket GhcPs) where
  prettyPrint (ExpBr _ expr) = brackets $ wrapWithBars $ prettyPrintWithComments expr
  prettyPrint (PatBr _ expr) =
    brackets $ do
      string "p"
      wrapWithBars $ prettyPrintWithComments expr
  prettyPrint DecBrL {} = undefined
  prettyPrint DecBrG {} = undefined
  prettyPrint (TypBr _ expr) =
    brackets $ do
      string "t"
      wrapWithBars $ prettyPrintWithComments expr
  prettyPrint (VarBr _ True var) = do
    string "'"
    prettyPrintWithComments var
  prettyPrint (VarBr _ False var) = do
    string "''"
    prettyPrintWithComments var
  prettyPrint TExpBr {} = undefined

instance PrettyPrintable SigMethodsFamily where
  prettyPrint (Sig x)        = prettyPrintWithComments x
  prettyPrint (Method x)     = prettyPrintWithComments x
  prettyPrint (TypeFamily x) = prettyPrintWithComments x

instance PrettyPrintable EpaComment where
  prettyPrint EpaComment {..} = prettyPrintWithComments ac_tok

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrintWithComments-print 'Anchor'.
instance PrettyPrintable Anchor where
  prettyPrint _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrintWithComments-print 'SrcAnn'.
instance PrettyPrintable (SrcAnn a) where
  prettyPrint _ = return ()
  commentsBefore (SrcSpanAnn ep _) = commentsBefore ep
  commentOnSameLine (SrcSpanAnn ep _) = commentOnSameLine ep
  commentsAfter (SrcSpanAnn ep _) = commentsAfter ep

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrintWithComments-print 'SrcSpan'.
instance PrettyPrintable SrcSpan where
  prettyPrint _ = return ()

-- FIXME: This instance declaration is wrong. The declaration exists only
-- for satisfying the 'GenLocated' one. We can't prettyPrintWithComments-print 'EpAnn'.
instance PrettyPrintable (EpAnn a) where
  prettyPrint _ = return ()
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
  prettyPrint (HsValBinds _ lr) = prettyPrintWithComments lr
  prettyPrint x                 = output x

instance PrettyPrintable (HsValBindsLR GhcPs GhcPs) where
  prettyPrint (ValBinds _ methods sigs) =
    lined $ fmap prettyPrintWithComments sigsAndMethods
      -- TODO: Merge this where clause with the one in the 'ClassDecl' of
      -- 'TyClDecl'.
    where
      sigsAndMethods =
        sortByLocation $ fmap Sig sigs ++ fmap Method (bagToList methods)
      sortByLocation = sortBy (compare `on` getLocation)
      getLocation (Sig x)       = realSrcSpan $ locA $ getLoc x
      getLocation (Method x)    = realSrcSpan $ locA $ getLoc x
      getLocation TypeFamily {} = undefined
  prettyPrint x = output x

instance PrettyPrintable (HsTupArg GhcPs) where
  prettyPrint (Present _ e) = prettyPrintWithComments e
  prettyPrint Missing {}    = return () -- This appears in a tuple section.

-- FIXME: Reconsider using a type variable. Using type variables may need
-- to define odd instances (e.g., Void).
instance (PrettyPrintable a, PrettyPrintable b) =>
         PrettyPrintable (HsRecField' a b) where
  prettyPrint HsRecField {..} = horizontal <-|> vertical
    where
      horizontal = do
        prettyPrintWithComments hsRecFieldLbl
        unless hsRecPun $ do
          string " = "
          prettyPrintWithComments hsRecFieldArg
      vertical = do
        prettyPrintWithComments hsRecFieldLbl
        unless hsRecPun $ do
          string " ="
          newline
          indentedBlock $ prettyPrintWithComments hsRecFieldArg

instance PrettyPrintable (FieldOcc GhcPs) where
  prettyPrint FieldOcc {..} = prettyPrintWithComments rdrNameFieldOcc

-- HsConDeclH98Details
instance PrettyPrintable (HsConDetails Void (HsScaled GhcPs (GenLocated SrcSpanAnnA (BangType GhcPs))) (GenLocated SrcSpanAnnL [GenLocated SrcSpanAnnA (ConDeclField GhcPs)])) where
  prettyPrint (PrefixCon _ xs) = horizontal <-|> vertical
    where
      horizontal = spacePrefixed $ fmap prettyPrintWithComments xs
      vertical = indentedBlock $ newlinePrefixed $ fmap prettyPrintWithComments xs
  prettyPrint (RecCon (L _ rec)) = do
    newline
    indentedBlock $ vFields $ fmap prettyPrintWithComments rec
  prettyPrint InfixCon {} =
    error
      "Cannot handle here because 'InfixCon' does not have the information of its constructor."

-- FIXME: Reconsider using a type variable.
instance PrettyPrintable a => PrettyPrintable (HsScaled GhcPs a) where
  prettyPrint (HsScaled _ x) = prettyPrintWithComments x

instance PrettyPrintable (ConDeclField GhcPs) where
  prettyPrint ConDeclField {..}
    -- Here, we *ignore* the 'cd_fld_doc' field because doc strings are
    -- also stored as comments, and printing both results in duplicated
    -- comments.
   = do
    prettyPrintWithComments $ head cd_fld_names
    string " :: "
    prettyPrintWithComments cd_fld_type

instance PrettyPrintable InfixExpr where
  prettyPrint (InfixExpr (L _ (HsVar _ bind))) = infixOp $ unLoc bind
  prettyPrint (InfixExpr x)                    = prettyPrint x
  commentsBefore (InfixExpr x) = commentsBefore x
  commentOnSameLine (InfixExpr x) = commentOnSameLine x
  commentsAfter (InfixExpr x) = commentsAfter x

instance PrettyPrintable InfixApp where
  prettyPrint InfixApp {..} = horizontal <-|> vertical
    where
      horizontal =
        spaced [prettyPrintWithComments lhs, prettyPrintWithComments (InfixExpr op), prettyPrintWithComments rhs]
      vertical = do
        lhsVer
        beforeRhs <-
          case unLoc lhs of
            (HsDo _ DoExpr {} _) -> do
              indentedWithSpace 3 (newline >> prettyPrintWithComments (InfixExpr op)) -- 3 for "do "
              return space
            _ -> do
              space
              prettyPrintWithComments (InfixExpr op)
              return newline
        (if immediatelyAfterDo
           then indentedBlock
           else id) $
          case unLoc rhs of
            (HsDo _ (DoExpr _) xs) -> do
              string " do"
              newline
              indentedBlock $ lined $ prettyPrintWithComments <$> unLoc xs -- TODO: Handle comments.
            HsLam {} -> do
              space
              prettyPrintWithComments rhs
            HsLamCase {} -> do
              space
              prettyPrintWithComments rhs
            _ -> do
              beforeRhs
              col <- startingColumn
              (if col == 0
                 then indentedBlock
                 else id) $
                prettyPrintWithComments rhs
      lhsVer =
        case lhs of
          (L loc (OpApp _ l o r)) ->
            prettyPrintWithComments (L loc (InfixApp l o r immediatelyAfterDo))
          _ -> prettyPrintWithComments lhs

instance PrettyPrintable a => PrettyPrintable (BooleanFormula a) where
  prettyPrint (Var x)    = prettyPrintWithComments x
  prettyPrint (And xs)   = commaSep $ fmap prettyPrintWithComments xs
  prettyPrint (Or xs)    = barSep $ fmap prettyPrintWithComments xs
  prettyPrint (Parens x) = parens $ prettyPrintWithComments x

instance PrettyPrintable (FieldLabelStrings GhcPs) where
  prettyPrint _ = undefined

instance PrettyPrintable (AmbiguousFieldOcc GhcPs) where
  prettyPrint (Unambiguous _ name) = prettyPrintWithComments name
  prettyPrint (Ambiguous _ name)   = prettyPrintWithComments name

instance PrettyPrintable (ImportDecl GhcPs) where
  prettyPrint ImportDecl {..} = do
    string "import "
    when (ideclSource == IsBoot) $ string "{-# SOURCE #-} "
    when ideclSafe $ string "safe "
    unless (ideclQualified == NotQualified) $ string "qualified "
    whenJust ideclPkgQual $ \x -> do
      prettyPrintWithComments x
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
  prettyPrint HsDerivingClause {..} = do
    string "deriving "
    whenJust deriv_clause_strategy $ \x -> do
      output x
      space
    prettyPrintWithComments deriv_clause_tys

instance PrettyPrintable (DerivClauseTys GhcPs) where
  prettyPrint (DctSingle _ ty) = parens $ prettyPrintWithComments ty
  prettyPrint (DctMulti _ ts)  = tuple $ fmap prettyPrintWithComments ts

instance PrettyPrintable OverlapMode where
  prettyPrint NoOverlap {}    = undefined
  prettyPrint Overlappable {} = undefined
  prettyPrint Overlapping {}  = string "{-# OVERLAPPING #-}"
  prettyPrint Overlaps {}     = undefined
  prettyPrint Incoherent {}   = undefined

instance PrettyPrintable StringLiteral where
  prettyPrint = output

instance PrettyPrintable (FamilyDecl GhcPs) where
  prettyPrint FamilyDecl {..} = do
    string "type "
    prettyPrintWithComments fdLName
    spacePrefixed $ prettyPrintWithComments <$> hsq_explicit fdTyVars
    string " = "
    prettyPrintWithComments fdResultSig
    whenJust fdInjectivityAnn $ \x -> do
      string " | "
      prettyPrintWithComments x

instance PrettyPrintable (FamilyResultSig GhcPs) where
  prettyPrint NoSig {}       = undefined
  prettyPrint KindSig {}     = undefined
  prettyPrint (TyVarSig _ x) = prettyPrintWithComments x

instance PrettyPrintable (HsTyVarBndr a GhcPs) where
  prettyPrint (UserTyVar _ _ x) = prettyPrintWithComments x
  prettyPrint KindedTyVar {}    = undefined

instance PrettyPrintable (InjectivityAnn GhcPs) where
  prettyPrint (InjectivityAnn _ from to) = do
    prettyPrintWithComments from
    string " -> "
    spaced $ fmap prettyPrintWithComments to

instance PrettyPrintable (ArithSeqInfo GhcPs) where
  prettyPrint (From from) =
    brackets $ do
      prettyPrintWithComments from
      string " .."
  prettyPrint FromThen {} = undefined
  prettyPrint FromTo {} = undefined
  prettyPrint FromThenTo {} = undefined

prefixExpr :: HsExpr GhcPs -> Printer ()
prefixExpr (HsVar _ bind) = prefixOp $ unLoc bind
prefixExpr x              = prettyPrintWithComments x
