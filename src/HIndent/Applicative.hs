module HIndent.Applicative
  ( whenJust
  ) where

whenJust :: (Applicative m) => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _  = pure ()
whenJust (Just x) f = f x
