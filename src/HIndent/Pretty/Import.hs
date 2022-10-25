-- | Helper functions for dealing with import declarations.
module HIndent.Pretty.Import
  ( groupImports
  ) where

import           GHC.Hs
import           GHC.Types.SrcLoc

-- | Combines adjacent import declarations into a single list.
groupImports :: [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
groupImports = groupImports' []
  where
    groupImports' ::
         [[LImportDecl GhcPs]] -> [LImportDecl GhcPs] -> [[LImportDecl GhcPs]]
    groupImports' xs [] = xs
    groupImports' [] (x:xs) = groupImports' [[x]] xs
    groupImports' [[]] (x:xs) = groupImports' [[x]] xs
    groupImports' ([]:x:xs) (y:ys) = groupImports' ([y] : x : xs) ys
    groupImports' ((z:zs):xs) (y:ys)
      | z `isAdjacentTo` y = groupImports' ((y : z : zs) : xs) ys
      | otherwise = groupImports' ([y] : (z : zs) : xs) ys
    a `isAdjacentTo` b =
      srcSpanEndLine (sp a) + 1 == srcSpanStartLine (sp b) ||
      srcSpanEndLine (sp b) + 1 == srcSpanStartLine (sp a)
    sp x =
      case locA $ getLoc x of
        RealSrcSpan x' _ -> x'
        _                -> error "Src span unavailable."
