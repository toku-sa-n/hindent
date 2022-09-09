module HIndent.Pretty.Pragma
  ( outputPragmas
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

outputPragmas :: HsModule -> Printer ()
outputPragmas = lined . fmap string . collectPragmas

pragmaExists :: HsModule -> Bool
pragmaExists = not . null . collectPragmas

-- TODO: This function collects pragmas from the entire module, and it
-- should be slow. Limit the range to search for them.
collectPragmas :: HsModule -> [String]
collectPragmas =
  mapMaybe unwrapComment . filter isPragma . listify matchToComment
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True
    unwrapComment (EpaBlockComment c) = Just c
    unwrapComment _                   = Nothing

isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ ("{-# +LANGUAGE +[a-zA-Z]+ +#-}" :: String)
isPragma _                   = False
