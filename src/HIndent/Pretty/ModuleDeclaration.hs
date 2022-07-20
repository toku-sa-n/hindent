module HIndent.Pretty.ModuleDeclaration
  ( printModuleDeclarationToPrinter
  ) where

import           Control.Monad
import           GHC.Hs
import           GHC.Types.SrcLoc           (GenLocated (..))
import           HIndent.Pretty.Combinators
import           HIndent.Types

printModuleDeclarationToPrinter :: HsModule -> Printer ()
printModuleDeclarationToPrinter HsModule {hsmodName = Nothing} = return ()
printModuleDeclarationToPrinter HsModule { hsmodName = Just name
                                         , hsmodExports = Nothing
                                         } = do
  string "module "
  printOutputableToPrinter name
  string " where"
  newline
  newline
printModuleDeclarationToPrinter HsModule { hsmodName = Just name
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
    newline
printModuleDeclarationToPrinter HsModule { hsmodName = Just name
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
    newline
