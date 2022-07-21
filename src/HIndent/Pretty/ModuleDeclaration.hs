module HIndent.Pretty.ModuleDeclaration
  ( outputModuleDeclaration
  ) where

import           Control.Monad
import           GHC.Hs
import           GHC.Types.SrcLoc           (GenLocated (..))
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputModuleDeclaration :: HsModule -> Printer ()
outputModuleDeclaration HsModule {hsmodName = Nothing} = return ()
outputModuleDeclaration HsModule {hsmodName = Just name, hsmodExports = Nothing} = do
  string "module "
  printOutputableToPrinter name
  string " where"
  newline
outputModuleDeclaration HsModule { hsmodName = Just name
                                 , hsmodExports = Just (L _ [])
                                 } = do
  string "module "
  printOutputableToPrinter name
  newline
  indentedBlock $ do
    string "("
    newline
    string ") where"
    newline
outputModuleDeclaration HsModule { hsmodName = Just name
                                 , hsmodExports = Just (L _ (x:xs))
                                 } = do
  string "module "
  printOutputableToPrinter name
  newline
  indentedBlock $ do
    string "( "
    printOutputableToPrinter x
    newline
    forM_ xs $ \e -> do
      string ", "
      printOutputableToPrinter e
      newline
    string ") where"
    newline
