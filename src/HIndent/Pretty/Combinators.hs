{-# LANGUAGE LambdaCase #-}

module HIndent.Pretty.Combinators
  ( string
  , space
  , newline
  , blankline
  , inter
  , spaced
  , collectComments
  , horizontalTuple
  , verticalTuple
  , indentedBlock
  , indentedWithSpace
  , indentedDependingOnHead
  , ifFitsOnOneLineOrElse
  , output
  , showOutputable
  , rhsSeparator
  , commentsArePrinted
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.RWS                                   hiding
                                                                     (state)
import qualified Data.ByteString.Builder                             as S
import           Data.Data
import           Data.Int
import           Data.List
import           Generics.SYB
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Utils.Outputable                                hiding
                                                                     (brackets,
                                                                      parens,
                                                                      space,
                                                                      (<>))
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

collectComments :: Data a => a -> [EpaCommentTok]
collectComments = listify (const True)

inter :: Printer () -> [Printer ()] -> Printer ()
inter separator = sequence_ . intersperse separator

spaced :: [Printer ()] -> Printer ()
spaced = inter space

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

getIndentSpaces :: Printer Int64
getIndentSpaces = gets (configIndentSpaces . psConfig)

rhsSeparator :: Printer ()
rhsSeparator = do
  isInsideCase <- gets psInsideCase
  isInsideLambda <- gets psInsideLambda
  string $
    if isInsideCase || isInsideLambda
      then "->"
      else "="

commentsArePrinted :: Printer ()
commentsArePrinted = modify (\s -> s {psEolComment = True})
