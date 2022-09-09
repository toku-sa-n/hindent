{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module HIndent.Pretty.Combinators
  ( string
  , space
  , newline
  , blankline
  , comma
  , inter
  , spaced
  , lined
  , horizontalTuple
  , verticalTuple
  , ifFitsOnOneLineOrElse
  , output
  , showOutputable
  , rhsSeparator
  , eolCommentsArePrinted
  , prefixedLined
  , startingColumn
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS                                   hiding
                                                                     (state)
import qualified Data.ByteString.Builder                             as S
import           Data.Int
import           Data.List
#if MIN_VERSION_ghc_lib_parser(9,2,2)
import           GHC.Driver.Ppr
#endif
import           GHC.Driver.Session
import           GHC.Utils.Outputable                                hiding
                                                                     (brackets,
                                                                      comma,
                                                                      parens,
                                                                      space,
                                                                      (<>))
import           HIndent.Pretty.Combinators.Indent
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
      out =
        if psNewline state && not writingNewline
          then replicate (fromIntegral $ psIndentLevel state) ' ' <> x
          else x
      psColumn' =
        if additionalLines > 0
          then fromIntegral $ length $ concat $ take 1 $ reverse srclines
          else psColumn state + fromIntegral (length out)
  when hardFail $
    guard $
    additionalLines == 0 && psColumn' <= configMaxColumns (psConfig state)
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

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator

spaced :: [Printer ()] -> Printer ()
spaced = inter space

lined :: [Printer ()] -> Printer ()
lined = inter newline

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

output :: Outputable a => a -> Printer ()
output = string . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

rhsSeparator :: Printer ()
rhsSeparator = do
  isInsideCase <- gets psInsideCase
  isInsideLambda <- gets psInsideLambda
  isInsideMultiwayIf <- gets psInsideMultiwayIf
  string $
    if isInsideCase || isInsideLambda || isInsideMultiwayIf
      then "->"
      else "="

eolCommentsArePrinted :: Printer ()
eolCommentsArePrinted = modify (\s -> s {psEolComment = True})

prefixedLined :: String -> [Printer ()] -> Printer ()
prefixedLined _ [] = return ()
prefixedLined pref (x:xs) = do
  x
  indentedWithSpace (fromIntegral (length pref * (-1))) $
    forM_ xs $ \p -> do
      newline
      indentedDependingOnHead (string pref) p

startingColumn :: Printer Int64
startingColumn = do
  before <- get
  string ""
  after <- get
  put before
  return $ psColumn after
