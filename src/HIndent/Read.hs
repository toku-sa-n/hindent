-- | 'read' implementation with a call stack and a clear message.
module HIndent.Read
  ( readOrFail
  ) where

import           Data.Maybe
import           GHC.Stack
import           Text.Read

-- | `read` but dumps a stack trace if reading a value fails.
readOrFail ::
     HasCallStack
  => Read a =>
       String -> a
readOrFail x = fromMaybe (error $ "`read` failed: " ++ x) $ readMaybe x
