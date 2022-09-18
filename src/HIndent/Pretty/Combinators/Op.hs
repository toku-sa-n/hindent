module HIndent.Pretty.Combinators.Op
  ( prefixOp
  , unlessSpecialOp
  ) where

import           Control.Monad
import           GHC.Types.Name
import           GHC.Types.Name.Reader
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Combinators.String
import           HIndent.Pretty.Combinators.Wrap
import           HIndent.Types

prefixOp :: RdrName -> Printer ()
prefixOp (Unqual name) = parensIfSymbol name $ output name
prefixOp (Qual modName name) =
  parensIfSymbol name $ do
    output modName
    string "."
    output name
prefixOp Orig {} = undefined
prefixOp (Exact name) = parensIfSymbol occ $ output occ
  where
    occ = occName name

unlessSpecialOp :: RdrName -> Printer () -> Printer ()
unlessSpecialOp name = unless (isSpecialOp name)

parensIfSymbol :: OccName -> Printer a -> Printer a
parensIfSymbol name
  | isSymOcc name = parens
  | otherwise = id

isSpecialOp :: RdrName -> Bool
isSpecialOp (Unqual name) = isSpecialOpString $ occNameString name
isSpecialOp Qual {}       = undefined
isSpecialOp Orig {}       = undefined
isSpecialOp (Exact name)  = isSpecialOpString $ occNameString $ nameOccName name

isSpecialOpString :: String -> Bool
isSpecialOpString name = name `elem` ["()", "[]", "->", ":"]
