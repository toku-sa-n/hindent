module HIndent.Pretty.Decls
  ( outputDecls
  , declsExist
  ) where

import           Data.Maybe
import           GHC.Hs
import           GHC.Types.SrcLoc
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputDecls :: HsModule -> Printer ()
outputDecls =
  mapM_ (\(x, sp) -> outputOutputable x >> fromMaybe (return ()) sp) .
  addSeparator . fmap unLoc . hsmodDecls

declsExist :: HsModule -> Bool
declsExist = not . null . hsmodDecls

addSeparator :: [HsDecl GhcPs] -> [(HsDecl GhcPs, Maybe (Printer ()))]
addSeparator []     = []
addSeparator [x]    = [(x, Nothing)]
addSeparator (x:xs) = (x, Just $ separator x) : addSeparator xs

separator :: HsDecl GhcPs -> Printer ()
separator SigD {} = newline
separator _       = blankline
