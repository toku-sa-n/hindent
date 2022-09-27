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

space :: Printer ()
space = string " "

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

blankline :: Printer ()
blankline = newline >> newline

comma :: Printer ()
comma = string ","

dot :: Printer ()
dot = string "."
