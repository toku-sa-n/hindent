{-# LANGUAGE LambdaCase #-}

module HIndent.Pretty.Combinators.Switch
  ( (<-|>)
  ) where

import           Control.Applicative
import           Control.Monad.State
import           HIndent.Types

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
