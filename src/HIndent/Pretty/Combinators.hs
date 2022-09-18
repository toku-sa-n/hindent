{-# LANGUAGE CPP        #-}
{-# LANGUAGE LambdaCase #-}

module HIndent.Pretty.Combinators
  ( (<-|>)
  , output
  , showOutputable
  , prefixed
  , eolCommentsArePrinted
  , startingColumn
  , printerLength
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

-- | Prints the text passed as the first argument before the current
-- position and then the second argument.
prefixed :: String -> Printer () -> Printer ()
prefixed s p = do
  indentedWithSpace (-(fromIntegral $ length s)) $ string s
  p

eolCommentsArePrinted :: Printer ()
eolCommentsArePrinted = modify (\s -> s {psEolComment = True})

startingColumn :: Printer Int64
startingColumn = do
  before <- get
  string ""
  after <- get
  put before
  return $ psColumn after

-- Returns how many characters the printer moved the cursor horizontally.
-- The returned value maybe negative if the printer prints multiple lines
-- and the column of the last position is less than before.
printerLength :: Printer a -> Printer Int64
printerLength p = do
  before <- get
  _ <- p
  after <- get
  put before
  pure $ psColumn after - psColumn before
