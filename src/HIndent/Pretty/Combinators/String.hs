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
import           HIndent.Types

-- | This function prints the given string.
--
-- The string should not include '\n's. Use 'newline' to print them.
--
-- FIXME: This function should forbid 'x' having '\n's, but it is
-- impossible because some functions call it with a string having them (I
-- think it is because of 'output').
string :: String -> Printer ()
string "\n" = error "Use `newline`."
string x = do
  eol <- gets psEolComment
  hardFail <- gets psFitOnOneLine
  when eol newline
  st <- get
  let out =
        if psNewline st
          then replicate (fromIntegral $ psIndentLevel st) ' ' <> x
          else x
      psColumn' =
        if additionalLines > 0
          then fromIntegral $ length $ last srclines
          else psColumn st + fromIntegral (length out)
  when hardFail $
    guard $ additionalLines == 0 && psColumn' <= configMaxColumns (psConfig st)
  modify
    (\s ->
       s
         { psOutput = psOutput st <> S.stringUtf8 out
         , psNewline = False
         , psLine = psLine st + fromIntegral additionalLines
         , psEolComment = False
         , psColumn = psColumn'
         })
  where
    srclines = lines x
    additionalLines = length $ filter (== '\n') x

-- | Equivalent to 'string " "'.
space :: Printer ()
space = string " "

-- | This function prints a '\n'.
--
-- Always call this function to print it because printing it requires
-- special handling. Do not call 'string' instead.
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
