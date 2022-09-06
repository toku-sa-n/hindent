module HIndent.Pretty.Combinators.Indent
  ( indentedBlock
  , indentedWithSpace
  , indentedDependingOnHead
  , indentedWithLevel
  , increaseIndentLevel
  , decreaseIndentLevel
  , getIndentSpaces
  ) where

import           Control.Monad.State
import           Data.Int
import           HIndent.Types

indentedBlock :: Printer a -> Printer a
indentedBlock p = do
  indentSpaces <- getIndentSpaces
  indentedWithSpace indentSpaces p

indentedWithSpace :: Int64 -> Printer a -> Printer a
indentedWithSpace i p = do
  level <- gets psIndentLevel
  modify (\s -> s {psIndentLevel = level + i})
  m <- p
  modify (\s -> s {psIndentLevel = level})
  return m

indentedDependingOnHead :: Printer () -> Printer a -> Printer a
indentedDependingOnHead hd p = do
  hd
  col <- gets psColumn
  indentedWithLevel col p

indentedWithLevel :: Int64 -> Printer a -> Printer a
indentedWithLevel i p = do
  l <- gets psIndentLevel
  modify (\s -> s {psIndentLevel = i})
  m <- p
  modify (\s -> s {psIndentLevel = l})
  return m

increaseIndentLevel :: Printer ()
increaseIndentLevel = do
  n <- getIndentSpaces
  modify (\s -> s {psIndentLevel = psIndentLevel s + n})

decreaseIndentLevel :: Printer ()
decreaseIndentLevel = do
  n <- getIndentSpaces
  modify (\s -> s {psIndentLevel = psIndentLevel s - n})

getIndentSpaces :: Printer Int64
getIndentSpaces = gets (configIndentSpaces . psConfig)
