{-# LANGUAGE LambdaCase #-}

module HIndent.Pretty.Combinators
  ( string
  , newline
  , blankline
  , inter
  , horizontalTuple
  , verticalTuple
  , indentedBlock
  , ifFitsOnOneLineOrElse
  , outputOutputable
  , showOutputable
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS                                   hiding
                                                                     (state)
import qualified Data.ByteString.Builder                             as S
import           Data.Int
import           Data.List
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

blankline :: Printer ()
blankline = newline >> newline

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator

horizontalTuple :: [Printer ()] -> Printer ()
horizontalTuple ps = do
  string "("
  inter (string ", ") ps
  string ")"

verticalTuple :: [Printer ()] -> Printer ()
verticalTuple ps = do
  string "( "
  inter (newline >> string ", ") ps
  newline
  string ")"

ifFitsOnOneLineOrElse :: Printer a -> Printer a -> Printer a
ifFitsOnOneLineOrElse fit notFit = do
  before <- get
  put before {psFitOnOneLine = True}
  fmap Just fit <|> return Nothing >>= \case
    Just r -> do
      modify $ \st -> st {psFitOnOneLine = psFitOnOneLine before}
      return r
    Nothing -> do
      put before
      guard $ not $ psFitOnOneLine before
      notFit

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
