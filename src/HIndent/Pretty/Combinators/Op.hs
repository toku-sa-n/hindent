module HIndent.Pretty.Combinators.Op
  ( infixOp
  , prefixOp
  ) where

import           Data.Char
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Types

infixOp :: RdrName -> Printer ()
infixOp (Unqual name) =
  case showOutputable name of
    [] -> error "The name is empty."
    s@(x:_) ->
      if isAlpha x
        then tick $ string s
        else string s
infixOp x = output x

prefixOp :: RdrName -> Printer ()
prefixOp (Unqual name) = prefixOutput $ occNameString name
prefixOp Qual {}       = undefined
prefixOp Orig {}       = undefined
prefixOp (Exact name)  = prefixOutput $ showOutputable name

prefixOutput :: String -> Printer ()
prefixOutput [] = error "The name is empty."
prefixOutput s@(x:_) =
  if isAlpha x
    then string s
    else parens $ string s
