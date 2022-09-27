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

collectPragmas :: HsModule -> [String]
collectPragmas =
  fmap (\x -> "{-# LANGUAGE " ++ x ++ " #-}") .
  mapMaybe fetchPragma . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True

fetchPragma :: EpaCommentTok -> Maybe String
fetchPragma (EpaBlockComment c) =
  case c =~ "{-# +LANGUAGE +([a-zA-Z0-9]+) +#-}" :: ( String
                                                    , String
                                                    , String
                                                    , [String]) of
    (_, _, _, [x]) -> Just x
    _              -> Nothing
fetchPragma _ = Nothing

isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ ("{-# +LANGUAGE +[a-zA-Z]+ +#-}" :: String)
isPragma _                   = False
