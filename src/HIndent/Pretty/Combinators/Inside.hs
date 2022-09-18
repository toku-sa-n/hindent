module HIndent.Pretty.Combinators.Inside
  ( insideConPat
  , insideInstDecl
  , insideDeclSig
  , insideVerticalList
  , insideVerticalFunctionSignature
  , exitVerticalSig
  , resetInside
  , isInsideConPat
  , isInsideDeclSig
  , isInsideInstDecl
  , isInsideVerticalFuncSig
  , isInsideVerticalList
  ) where

import           Control.Monad.RWS
import           Data.Set
import           HIndent.Types

insideConPat :: Printer a -> Printer a
insideConPat = inside InsideConPat

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

isInsideConPat :: Printer Bool
isInsideConPat = isInside InsideConPat

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
