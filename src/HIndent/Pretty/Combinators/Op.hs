module HIndent.Pretty.Combinators.Op
  ( infixOp
  , prefixOp
  , unlessSpecialOp
  ) where

import           Control.Monad
import           Data.Char
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           GHC.Unit
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.String
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
prefixOp (Qual modName name) =
  prefixOutputWithModuleName (moduleNameString modName) (occNameString name)
prefixOp Orig {} = undefined
prefixOp (Exact name) = prefixOutput $ showOutputable name

unlessSpecialOp :: RdrName -> Printer () -> Printer ()
unlessSpecialOp name = unless (isSpecialOp name)

prefixOutput :: String -> Printer ()
prefixOutput [] = error "The name is empty."
prefixOutput s@(x:_) =
  if isAlpha x || s `elem` ["()", "[]"]
    then string s
    else parens $ string s

prefixOutputWithModuleName :: String -> String -> Printer ()
prefixOutputWithModuleName [] _ = error "The module name is empty."
prefixOutputWithModuleName _ [] = error "The name is empty."
prefixOutputWithModuleName m s@(x:_) =
  if isAlpha x
    then string $ m ++ "." ++ s
    else parens $ string $ m ++ "." ++ s

isSpecialOp :: RdrName -> Bool
isSpecialOp (Unqual name) = isSpecialOpString $ occNameString name
isSpecialOp (Qual _ _)    = undefined
isSpecialOp Orig {}       = undefined
isSpecialOp (Exact name)  = isSpecialOpString $ occNameString $ nameOccName name

isSpecialOpString :: String -> Bool
isSpecialOpString name = name `elem` ["()", "[]", "->", ":"]
