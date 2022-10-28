-- | Pretty-printing pragmas and 'GHC_OPTIONS'
--
-- TODO: Merge processes with 'HIndent.Extension'.
module HIndent.Pretty.Pragma
  ( prettyPragmas
  , pragmaExists
  , isPragma
  ) where

import           Data.Bifunctor
import           Data.Char
import           Data.Generics.Schemes
import           Data.List
import           Data.List.Split
import           Data.Maybe
import           GHC.Hs
import           HIndent.Pragma
import           HIndent.Pretty.Combinators.Lineup
import           HIndent.Pretty.Combinators.String
import           HIndent.Types
import           Text.Regex.TDFA

-- | This function pretty-prints the module's pragmas and 'GHC_OPTIONS'.
prettyPragmas :: HsModule -> Printer ()
prettyPragmas = lined . fmap string . collectPragmas

-- | This function returns a 'True' if the module has pragmas and
-- 'GHC_OPTIONS'. Otherwise, it returns a 'False'.
pragmaExists :: HsModule -> Bool
pragmaExists = not . null . collectPragmas

-- | This function collects pragma comments and 'GHC_OPTIONS' from the
-- given module and modifies them into 'String's.
collectPragmas :: HsModule -> [String]
collectPragmas =
  fmap (uncurry constructPragma) .
  mapMaybe extractPragma . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment EpaBlockComment {} = True
    matchToComment _                  = False
    constructPragma optionOrPragma xs =
      "{-# " ++ optionOrPragma ++ " " ++ intercalate ", " xs ++ " #-}"

-- | This function returns a 'Just' value with the pragma or 'GHC_OPTIONS'
-- extracted from the passed 'EpaCommentTok' if it has one. Otherwise, it
-- returns a 'Nothing'.
extractPragma :: EpaCommentTok -> Maybe (String, [String])
extractPragma (EpaBlockComment c) =
  second (fmap strip . splitOn ",") <$> extractPragmaNameAndElement c
  where
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
extractPragma _ = Nothing

-- | This function returns a 'True' if the passed 'EpaCommentTok' is
-- a pragma or a 'GHC_OPTIONS'. Otherwise, it returns a 'False'.
isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = match pragmaRegex c
isPragma _                   = False
