-- | Pragma AST.
{-# LANGUAGE CPP #-}

module HIndent.Ast.Pragma
  ( Pragma
  , mkPragmas
  , isPragma
  ) where

import Data.Bifunctor
import Data.Char
import Data.Generics.Schemes
import Data.List
import Data.List.Split
import Data.Maybe
import GHC.Hs
import HIndent.Pragma
import HIndent.Pretty
import HIndent.Pretty.Combinators.String
import HIndent.Pretty.NodeComments
import HIndent.Pretty.Types
import Text.Regex.TDFA

newtype Pragma =
  Pragma String

instance CommentExtraction Pragma where
  nodeComments (Pragma _) = NodeComments [] [] []

instance Pretty Pragma where
  pretty' (Pragma s) = string s
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
type HsModule' = HsModule GhcPs
#else
type HsModule' = HsModule
#endif
mkPragmas :: HsModule' -> [Pragma]
mkPragmas =
  fmap (Pragma . uncurry constructPragma)
    . mapMaybe extractPragma
    . listify isBlockComment
    . getModuleAnn

-- | This function returns a 'Just' value with the pragma
-- extracted from the passed 'EpaCommentTok' if it has one. Otherwise, it
-- returns a 'Nothing'.
extractPragma :: EpaCommentTok -> Maybe (String, [String])
extractPragma (EpaBlockComment c) =
  second (fmap strip . splitOn ",") <$> extractPragmaNameAndElement c
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
extractPragma _ = Nothing

-- | Construct a pragma.
constructPragma :: String -> [String] -> String
constructPragma optionOrPragma xs =
  "{-# " ++ fmap toUpper optionOrPragma ++ " " ++ intercalate ", " xs ++ " #-}"

-- | Checks if the given comment is a block one.
isBlockComment :: EpaCommentTok -> Bool
isBlockComment EpaBlockComment {} = True
isBlockComment _ = False

-- | This function returns a 'True' if the passed 'EpaCommentTok' is
-- a pragma. Otherwise, it returns a 'False'.
isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = match pragmaRegex c
isPragma _ = False

getModuleAnn :: HsModule' -> EpAnn AnnsModule
#if MIN_VERSION_ghc_lib_parser(9, 6, 1)
getModuleAnn = hsmodAnn . hsmodExt
#else
getModuleAnn = hsmodAnn
#endif
