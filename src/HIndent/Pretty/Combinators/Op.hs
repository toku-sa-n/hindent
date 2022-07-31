module HIndent.Pretty.Combinators.Op
  ( infixOp
  , prefixOp
  ) where

import           Data.Char
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
prefixOp (Unqual name) =
  case showOutputable name of
    [] -> error "The name is empty."
    s@(x:_) ->
      if isAlpha x
        then string s
        else parens $ string s
prefixOp x = output x
