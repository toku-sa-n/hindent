module HIndent.Pretty.Combinators.Inside
  ( insideDeclSig
  , resetInside
  , isInsideDeclSig
  ) where

import           Control.Monad.RWS
import           Data.Set
import           HIndent.Types

insideDeclSig :: Printer a -> Printer a
insideDeclSig = inside InsideDeclSig

inside :: Inside -> Printer b -> Printer b
inside v = modifyInsideSetTemporarily (insert v)

resetInside :: Printer b -> Printer b
resetInside = modifyInsideSetTemporarily (const empty)

modifyInsideSetTemporarily ::
     (Set Inside -> Set Inside) -> Printer a -> Printer a
modifyInsideSetTemporarily f p = do
  before <- gets psInside
  modify (\s -> s {psInside = f before})
  r <- p
  modify (\s -> s {psInside = before})
  pure r

isInsideDeclSig :: Printer Bool
isInsideDeclSig = isInside InsideDeclSig

isInside :: Inside -> Printer Bool
isInside x = gets ((x `elem`) . psInside)
