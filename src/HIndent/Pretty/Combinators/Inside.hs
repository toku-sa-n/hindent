module HIndent.Pretty.Combinators.Inside
  ( insideCase
  , insideLambda
  , insideSignature
  , insideVerticalList
  , whenInsideCase
  , whenInsideLambda
  , whenInsideSignature
  , unlessInsideLambda
  ) where

import           Control.Monad.RWS
import           HIndent.Types

insideCase :: Printer a -> Printer a
insideCase p = do
  before <- gets psInsideCase
  modify (\s -> s {psInsideCase = True})
  r <- p
  modify (\s -> s {psInsideCase = before})
  return r

insideLambda :: Printer a -> Printer a
insideLambda p = do
  before <- gets psInsideLambda
  modify (\s -> s {psInsideLambda = True})
  r <- p
  modify (\s -> s {psInsideLambda = before})
  return r

insideSignature :: Printer a -> Printer a
insideSignature p = do
  before <- gets psInsideSignature
  modify (\s -> s {psInsideSignature = True})
  r <- p
  modify (\s -> s {psInsideSignature = before})
  return r

insideVerticalList :: Printer a -> Printer a
insideVerticalList p = do
  before <- gets psInsideVerticalList
  modify (\s -> s {psInsideVerticalList = True})
  r <- p
  modify (\s -> s {psInsideVerticalList = before})
  return r

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
  -> Printer a
  -> Printer a
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
