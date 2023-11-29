-- | Module type.
{-# LANGUAGE CPP             #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module HIndent.Ast.Module
  ( Module(..)
  , mkModule
  ) where

import           GHC.Hs                   hiding (comments)
import qualified GHC.Hs                   as GHC
import           GHC.Types.SrcLoc
import           HIndent.Ast.WithComments
import           HIndent.Pretty.Pragma
import           HIndent.Pretty.Types
#if MIN_VERSION_ghc_lib_parser(9,6,1)
import           GHC.Core.DataCon
#endif

#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GHC.GhcPs
#else
type HsModule' = HsModule
#endif
data Module = Module
  { name    :: Maybe (WithComments String)
  , module' :: HsModule'
  }

mkModule :: HsModule' -> WithComments Module
mkModule m =
  WithComments {comments = epas m, node = Module {name = Nothing, module' = m}}
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
