module HIndent.PrettyPrint.Combinators.Inside
  ( insideCase
  , insideInstDecl
  , insideLambda
  , insideMultiwayIf
  , insideSignature
  , insideVerticalList
  , insideVerticalFunctionSignature
  , exitCase
  , exitLambda
  , exitVerticalFunctionSignature
  , whenInsideLambda
  , unlessInsideLambda
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

insideSignature :: Printer a -> Printer a
insideSignature = inside InsideSignature

insideVerticalList :: Printer a -> Printer a
insideVerticalList = inside InsideVerticalList

insideVerticalFunctionSignature :: Printer a -> Printer a
insideVerticalFunctionSignature = inside InsideVerticalFunctionSignature

exitCase :: Printer a -> Printer a
exitCase = exit InsideCase

exitLambda :: Printer a -> Printer a
exitLambda = exit InsideLambda

exitVerticalFunctionSignature :: Printer a -> Printer a
exitVerticalFunctionSignature = exit InsideVerticalFunctionSignature

whenInsideLambda :: Printer () -> Printer ()
whenInsideLambda = whenInside InsideLambda

unlessInsideLambda :: Printer () -> Printer ()
unlessInsideLambda = unlessInside InsideLambda

inside :: Inside -> Printer b -> Printer b
inside v = modifyInsideSetTemporarily (insert v)

exit :: Inside -> Printer b -> Printer b
exit v = modifyInsideSetTemporarily (delete v)

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
