module HIndent.Pretty.Combinators.Switch
  ( (<-|>)
  ) where

import           Control.Monad.State
import           HIndent.Types

(<-|>) :: Printer a -> Printer a -> Printer a
fit <-|> notFit = do
  before <- get
  r <- fit
  after <- get
  if isFit before after
    then put after >> pure r
    else put before >> notFit
  where
    isFit before after =
      psLine after <= psLine before &&
      psColumn after <= configMaxColumns (psConfig after)
