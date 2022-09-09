{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module HIndent.Pretty.Combinators
  ( (<-|>)
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
import           Data.Int
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
import           HIndent.Pretty.Combinators.String
import           HIndent.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config

(<-|>) :: Printer a -> Printer a -> Printer a
fit <-|> notFit = do
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
