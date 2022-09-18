module HIndent.Pretty.Combinators.Op
  ( unlessSpecialOp
  ) where

import           Control.Monad
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           HIndent.Types

unlessSpecialOp :: RdrName -> Printer () -> Printer ()
unlessSpecialOp name = unless (isSpecialOp name)

isSpecialOp :: RdrName -> Bool
isSpecialOp (Unqual name) = isSpecialOpString $ occNameString name
isSpecialOp Qual {}       = undefined
isSpecialOp Orig {}       = undefined
isSpecialOp (Exact name)  = isSpecialOpString $ occNameString $ nameOccName name

isSpecialOpString :: String -> Bool
isSpecialOpString name = name `elem` ["()", "[]", "->", ":"]
