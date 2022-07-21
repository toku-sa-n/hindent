module HIndent.Pretty.Pragma
  ( outputPragmas
  ) where

import           Data.Generics.Schemes
import           Data.Maybe
import           GHC.Hs
import           HIndent.Pretty.Combinators
import           HIndent.Types
import           Text.Regex.TDFA

outputPragmas :: HsModule -> Printer ()
outputPragmas m =
  case collectPragmas m of
    [] -> return ()
    xs -> do
      mapM_ string xs
      newline
      newline

collectPragmas :: HsModule -> [String]
collectPragmas =
  mapMaybe unwrapComment . filter isPragma . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True
    unwrapComment (EpaBlockComment c) = Just c
    unwrapComment _                   = Nothing

isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ ("{-# +LANGUAGE +[a-zA-Z]+ +#-}" :: String)
isPragma _                   = False
