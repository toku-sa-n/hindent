{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           Generics.SYB
import           GHC.Hs
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Decls
import           HIndent.Pretty.Imports
import           HIndent.Pretty.ModuleDeclaration
import           HIndent.Pretty.Pragma
import           HIndent.Types

-- | Pretty print including comments.
pretty :: HsModule -> Printer ()
pretty m = do
  inter blankline $ printers m
  printCommentsAtTheEndOfModule m

printers :: HsModule -> [Printer ()]
printers m = snd <$> filter fst pairs
  where
    pairs =
      [ (pragmaExists m, outputPragmas m)
      , (moduleDeclarationExists m, outputModuleDeclaration m)
      , (importsExist m, outputImports m)
      , (declsExist m, outputDecls m)
      ]

printCommentsAtTheEndOfModule :: HsModule -> Printer ()
printCommentsAtTheEndOfModule =
  inter newline .
  fmap printComment . filter (not . isPragma) . listify (const True) . hsmodAnn
