module HIndent.Pretty.Combinators.Inside
  ( insideInstDecl
  , insideDeclSig
  , insideVerticalList
  , insideVerticalFunctionSignature
  , exitVerticalSig
  , resetInside
  , isInsideDeclSig
  , isInsideInstDecl
  , isInsideVerticalFuncSig
  , isInsideVerticalList
  ) where

import           Control.Monad.RWS
import           Data.Set
import           HIndent.Types

insideInstDecl :: Printer a -> Printer a
insideInstDecl = inside InsideInstDecl

insideDeclSig :: Printer a -> Printer a
insideDeclSig = inside InsideDeclSig

insideVerticalList :: Printer a -> Printer a
insideVerticalList = inside InsideVerticalList

insideVerticalFunctionSignature :: Printer a -> Printer a
insideVerticalFunctionSignature = inside InsideVerticalFunctionSignature

inside :: Inside -> Printer b -> Printer b
inside v = modifyInsideSetTemporarily (insert v)

exitVerticalSig :: Printer a -> Printer a
exitVerticalSig =
  modifyInsideSetTemporarily (delete InsideVerticalFunctionSignature)

resetInside :: Printer b -> Printer b
resetInside = modifyInsideSetTemporarily (const empty)

modifyInsideSetTemporarily ::
     (Set Inside -> Set Inside) -> Printer a -> Printer a
modifyInsideSetTemporarily f p = do
  before <- gets psInside
  modify (\s -> s {psInside = f before})
  r <- p
  modify (\s -> s {psInside = before})
  pure r

isInsideDeclSig :: Printer Bool
isInsideDeclSig = isInside InsideDeclSig

isInsideInstDecl :: Printer Bool
isInsideInstDecl = isInside InsideInstDecl

isInsideVerticalFuncSig :: Printer Bool
isInsideVerticalFuncSig = isInside InsideVerticalFunctionSignature

isInsideVerticalList :: Printer Bool
isInsideVerticalList = isInside InsideVerticalList

isInside :: Inside -> Printer Bool
isInside x = gets ((x `elem`) . psInside)
