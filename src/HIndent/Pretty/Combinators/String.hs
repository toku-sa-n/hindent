{-# LANGUAGE ViewPatterns #-}

-- | Printer combinators related to print strings.
module HIndent.Pretty.Combinators.String
  ( string
  , space
  , newline
  , blankline
  , comma
  , dot
  ) where

import           Control.Monad.RWS
import qualified Data.ByteString.Builder as S
import           GHC.Stack
import           HIndent.Types

-- | This function prints the given string.
--
-- The string must not include '\n's. Use 'newline' to print them.
string :: HasCallStack => String -> Printer ()
string (('\n' `elem`) -> True) = error "Use `newline` to print '\\n's."
string x = do
  eol <- gets psEolComment
  hardFail <- gets psFitOnOneLine
  when eol newline
  st <- get
  let out =
        if psNewline st
          then replicate (fromIntegral $ psIndentLevel st) ' ' <> x
          else x
      psColumn' = psColumn st + fromIntegral (length out)
  when hardFail $ guard $ psColumn' <= configMaxColumns (psConfig st)
  modify
    (\s ->
       s
         { psOutput = psOutput st <> S.stringUtf8 out
         , psNewline = False
         , psEolComment = False
         , psColumn = psColumn'
         })

-- | Equivalent to 'string " "'.
space :: Printer ()
space = string " "

-- | This function prints a '\n'.
--
-- Always call this function to print it because printing it requires
-- special treatment. Do not call 'string' instead.
newline :: Printer ()
newline = do
  gets psFitOnOneLine >>= guard . not
  modify
    (\s ->
       s
         { psOutput = psOutput s <> S.stringUtf8 "\n"
         , psNewline = True
         , psLine = psLine s + 1
         , psEolComment = False
         , psColumn = 0
         })

-- | Equivalent to 'newline >> newline'.
blankline :: Printer ()
blankline = newline >> newline

-- | Equivalent to 'string ","'.
comma :: Printer ()
comma = string ","

-- | Equivalent to 'string "."'.
dot :: Printer ()
dot = string "."
