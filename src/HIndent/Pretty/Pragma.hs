-- | Pretty-printing pragmas
module HIndent.Pretty.Pragma
  ( prettyPragmas
  , pragmaExists
  , isPragma
  ) where

import           Data.Generics.Schemes
import           Data.Maybe
import           GHC.Hs
import           HIndent.Pretty.Combinators.Lineup
import           HIndent.Pretty.Combinators.String
import           HIndent.Types
import           Text.Regex.TDFA

-- | This function pretty-prints the module's pragmas.
prettyPragmas :: HsModule -> Printer ()
prettyPragmas = lined . fmap string . collectPragmas

-- | This function returns a 'True' if the module has pragmas. Otherwise,
-- it returns a 'False'.
pragmaExists :: HsModule -> Bool
pragmaExists = not . null . collectPragmas

collectPragmas :: HsModule -> [String]
collectPragmas =
  fmap (\x -> "{-# LANGUAGE " ++ x ++ " #-}") .
  mapMaybe fetchPragma . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True

fetchPragma :: EpaCommentTok -> Maybe String
fetchPragma (EpaBlockComment c) =
  case regexResult of
    (_, _, _, [x]) -> Just x
    _              -> Nothing
  where
    regexResult = c =~ pragmaRegex :: (String, String, String, [String])
fetchPragma _ = Nothing

isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ pragmaRegex
isPragma _                   = False

pragmaRegex :: String
pragmaRegex = "{-# +LANGUAGE +([a-zA-Z0-9]+) +#-}"
