module HIndent.Pretty.Combinators.Inside
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
import           HIndent.Types

insideCase :: Printer a -> Printer a
insideCase = inside psInsideCase (\a s -> s {psInsideCase = a}) True

insideInstDecl :: Printer a -> Printer a
insideInstDecl = inside psInsideInstDecl (\a s -> s {psInsideInstDecl = a}) True

insideMultiwayIf :: Printer a -> Printer a
insideMultiwayIf =
  inside psInsideMultiwayIf (\a s -> s {psInsideMultiwayIf = a}) True

insideLambda :: Printer a -> Printer a
insideLambda = inside psInsideLambda (\a s -> s {psInsideLambda = a}) True

insideSignature :: Printer a -> Printer a
insideSignature =
  inside psInsideSignature (\a s -> s {psInsideSignature = a}) True

insideVerticalList :: Printer a -> Printer a
insideVerticalList =
  inside psInsideVerticalList (\a s -> s {psInsideVerticalList = a}) True

insideVerticalFunctionSignature :: Printer a -> Printer a
insideVerticalFunctionSignature =
  inside
    psInsideVerticalFunctionSignature
    (\a s -> s {psInsideVerticalFunctionSignature = a})
    True

exitCase :: Printer a -> Printer a
exitCase = inside psInsideCase (\a s -> s {psInsideCase = a}) False

exitLambda :: Printer a -> Printer a
exitLambda = inside psInsideLambda (\a s -> s {psInsideLambda = a}) False

exitVerticalFunctionSignature :: Printer a -> Printer a
exitVerticalFunctionSignature =
  inside
    psInsideVerticalFunctionSignature
    (\a s -> s {psInsideVerticalFunctionSignature = a})
    False

whenInsideLambda :: Printer () -> Printer ()
whenInsideLambda = whenInside psInsideLambda

unlessInsideLambda :: Printer () -> Printer ()
unlessInsideLambda = unlessInside psInsideLambda

inside ::
     (PrintState -> a)
  -> (a -> PrintState -> PrintState)
  -> a
  -> Printer b
  -> Printer b
inside getter setter v p = do
  before <- gets getter
  modify (setter v)
  r <- p
  modify (setter before)
  return r

whenInside :: (PrintState -> Bool) -> Printer () -> Printer ()
whenInside f p = gets f >>= flip when p

unlessInside :: (PrintState -> Bool) -> Printer () -> Printer ()
unlessInside f p = gets f >>= flip unless p
