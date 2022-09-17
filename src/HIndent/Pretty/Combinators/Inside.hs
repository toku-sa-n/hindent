module HIndent.Pretty.Combinators.Inside
  ( insideCase
  , insideInstDecl
  , insideLambda
  , insideMultiwayIf
  , insideDeclSig
  , insideVerticalList
  , insideVerticalFunctionSignature
  , exitVerticalSig
  , resetInside
  , whenInsideLambda
  , unlessInsideLambda
  , isInsideDeclSig
  , isInsideInstDecl
  , isInsideVerticalFuncSig
  ) where

import           Control.Monad.RWS
import           Data.Set
import           HIndent.Types

insideCase :: Printer a -> Printer a
insideCase = inside InsideCase

insideInstDecl :: Printer a -> Printer a
insideInstDecl = inside InsideInstDecl

insideMultiwayIf :: Printer a -> Printer a
insideMultiwayIf = inside InsideMultiwayIf

insideLambda :: Printer a -> Printer a
insideLambda = inside InsideLambda

insideDeclSig :: Printer a -> Printer a
insideDeclSig = inside InsideDeclSig

insideVerticalList :: Printer a -> Printer a
insideVerticalList = inside InsideVerticalList

insideVerticalFunctionSignature :: Printer a -> Printer a
insideVerticalFunctionSignature = inside InsideVerticalFunctionSignature

whenInsideLambda :: Printer () -> Printer ()
whenInsideLambda = whenInside InsideLambda

unlessInsideLambda :: Printer () -> Printer ()
unlessInsideLambda = unlessInside InsideLambda

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

whenInside :: Inside -> Printer () -> Printer ()
whenInside i p = do
  set <- gets psInside
  when (i `elem` set) p

unlessInside :: Inside -> Printer () -> Printer ()
unlessInside i p = do
  set <- gets psInside
  unless (i `elem` set) p

isInsideDeclSig :: Printer Bool
isInsideDeclSig = gets ((InsideDeclSig `elem`) . psInside)

isInsideInstDecl :: Printer Bool
isInsideInstDecl = gets ((InsideInstDecl `elem`) . psInside)

isInsideVerticalFuncSig :: Printer Bool
isInsideVerticalFuncSig =
  gets ((InsideVerticalFunctionSignature `elem`) . psInside)
