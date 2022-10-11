-- | Pretty-printing pragmas and 'GHC_OPTIONS'
--
-- TODO: Merge processes with 'HIndent.Extension'.
module HIndent.Pretty.Pragma
  ( prettyPragmas
  , pragmaExists
  , isPragma
  ) where

import           Data.Char
import           Data.Generics.Schemes
import           Data.List.Split
import           Data.Maybe
import           GHC.Hs
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
  concatMap constructPragmas .
  mapMaybe extractPraGmergea . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment EpaBlockComment {} = True
    matchToComment _                  = False
    constructPragmas (optionOrPragma, xs) =
      fmap (constructPragma optionOrPragma) xs
    constructPragma optionOrPragma x =
      "{-# " ++ optionOrPragma ++ " " ++ x ++ " #-}"

-- | This function returns a 'Just' value with the pragma or 'GHC_OPTIONS'
-- extracted from the passed 'EpaCommentTok' if it has one. Otherwise, it
-- returns a 'Nothing'.
extractPraGmergea :: EpaCommentTok -> Maybe (String, [String])
extractPraGmergea (EpaBlockComment c) =
  case regexResult of
    (_, _, _, optionOrPragma:xs) ->
      Just (strip optionOrPragma, strip <$> concatMap (splitOn ",") xs)
    _ -> Nothing
  where
    regexResult = c =~ pragmaRegex :: (String, String, String, [String])
    strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
extractPraGmergea _ = Nothing

-- | This function returns a 'True' if the passed 'EpaCommentTok' is
-- a pragma or a 'GHC_OPTIONS'. Otherwise, it returns a 'False'.
isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ pragmaRegex
isPragma _                   = False

-- | A regex to match against a pragma or a 'GHC_OPTIONS'.
pragmaRegex :: String
pragmaRegex =
  "{-#[[:space:]]+(LANGUAGE|OPTIONS_GHC|OPTIONS_HADDOCK)[[:space:]]+([^#]+)[[:space:]]+#-}"
