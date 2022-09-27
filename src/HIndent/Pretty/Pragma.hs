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

-- | This function collects pragma comments from the given module and
-- modifies them into 'String's.
collectPragmas :: HsModule -> [String]
collectPragmas =
  fmap (\x -> "{-# LANGUAGE " ++ x ++ " #-}") .
  mapMaybe extractPraGmergea . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True

-- | This function returns a 'Just' value with the pragma extracted from
-- the passed 'EpaCommentTok' if it has one. Otherwise, it returns
-- a 'Nothing'.
extractPraGmergea :: EpaCommentTok -> Maybe String
extractPraGmergea (EpaBlockComment c) =
  case regexResult of
    (_, _, _, [x]) -> Just x
    _              -> Nothing
  where
    regexResult = c =~ pragmaRegex :: (String, String, String, [String])
extractPraGmergea _ = Nothing

-- | This function returns a 'True' if the passed 'EpaCommentTok' is
-- a pragma. Otherwise, it returns a 'False'.
isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ pragmaRegex
isPragma _                   = False

-- | A regex to match against a pragma.
pragmaRegex :: String
pragmaRegex = "{-# +LANGUAGE +([a-zA-Z0-9]+) +#-}"
