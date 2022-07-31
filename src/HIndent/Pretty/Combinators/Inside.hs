module HIndent.Pretty.Combinators.Inside
  ( insideCase
  , insideLambda
  , insideSignature
  , insideVerticalList
  , insideVerticalFunctionSignature
  , whenInsideCase
  , whenInsideLambda
  , whenInsideSignature
  , unlessInsideLambda
  ) where

import           Control.Monad.RWS
import           HIndent.Types

insideCase :: Printer a -> Printer a
insideCase = inside psInsideCase (\a s -> s {psInsideCase = a}) True

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

whenInsideCase :: Printer () -> Printer ()
whenInsideCase = whenInside psInsideCase

whenInsideLambda :: Printer () -> Printer ()
whenInsideLambda = whenInside psInsideLambda

whenInsideSignature :: Printer () -> Printer ()
whenInsideSignature = whenInside psInsideSignature

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
