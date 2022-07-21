module HIndent.Pretty.Combinators
  ( string
  , newline
  , indentedBlock
  , outputOutputable
  ) where

import           Control.Monad
import           Control.Monad.RWS                                   hiding
                                                                     (state)
import qualified Data.ByteString.Builder                             as S
import           Data.Int
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Utils.Outputable                                hiding
                                                                     ((<>))
import           HIndent.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config

string :: String -> Printer ()
string x = do
  eol <- gets psEolComment
  hardFail <- gets psFitOnOneLine
  let addingNewline = eol && x /= "\n"
  when addingNewline newline
  state <- get
  let writingNewline = x == "\n"
      out :: String
      out =
        if psNewline state && not writingNewline
          then replicate (fromIntegral (psIndentLevel state)) ' ' <> x
          else x
      psColumn' =
        if additionalLines > 0
          then fromIntegral (length (concat (take 1 (reverse srclines))))
          else psColumn state + fromIntegral (length out)
  when
    hardFail
    (guard
       (additionalLines == 0 && psColumn' <= configMaxColumns (psConfig state)))
  modify
    (\s ->
       s
         { psOutput = psOutput state <> S.stringUtf8 out
         , psNewline = False
         , psLine = psLine state + fromIntegral additionalLines
         , psEolComment = False
         , psColumn = psColumn'
         })
  where
    srclines = lines x
    additionalLines = length (filter (== '\n') x)

newline :: Printer ()
newline = do
  string "\n"
  modify (\s -> s {psNewline = True})

outputOutputable :: Outputable a => a -> Printer ()
outputOutputable = string . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

indentedBlock :: Printer a -> Printer a
indentedBlock p = do
  indentSpaces <- getIndentSpaces
  indented indentSpaces p

indented :: Int64 -> Printer a -> Printer a
indented i p = do
  level <- gets psIndentLevel
  modify (\s -> s {psIndentLevel = level + i})
  m <- p
  modify (\s -> s {psIndentLevel = level})
  return m

getIndentSpaces :: Printer Int64
getIndentSpaces = gets (configIndentSpaces . psConfig)
