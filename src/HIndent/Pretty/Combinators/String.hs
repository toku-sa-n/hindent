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
string x = do
  eol <- gets psEolComment
  let addingNewline = eol && x /= "\n"
  when addingNewline newline
  st <- get
  let writingNewline = x == "\n"
      out =
        if psNewline st && not writingNewline
          then replicate (fromIntegral $ psIndentLevel st) ' ' <> x
          else x
      psColumn' =
        if additionalLines > 0
          then fromIntegral $ length $ concat $ take 1 $ reverse srclines
          else psColumn st + fromIntegral (length out)
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
  string "\n"
  modify (\s -> s {psNewline = True})

blankline :: Printer ()
blankline = newline >> newline

comma :: Printer ()
comma = string ","

dot :: Printer ()
dot = string "."
