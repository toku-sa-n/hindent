{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           GHC.Hs
import           HIndent.Pretty.Combinators
import           HIndent.Pretty.Decls
import           HIndent.Pretty.Imports
import           HIndent.Pretty.ModuleDeclaration
import           HIndent.Pretty.Pragma
import           HIndent.Types

-- | Pretty print including comments.
pretty :: HsModule -> Printer ()
pretty = inter blankline . printers

printers :: HsModule -> [Printer ()]
printers m = snd <$> filter fst pairs
  where
    pairs =
      [ (pragmaExists m, outputPragmas m)
      , (moduleDeclarationExists m, outputModuleDeclaration m)
      , (importsExist m, outputImports m)
      , (declsExist m, outputDecls m)
      ]
