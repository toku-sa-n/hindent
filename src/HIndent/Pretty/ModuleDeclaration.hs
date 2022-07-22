module HIndent.Pretty.ModuleDeclaration
  ( outputModuleDeclaration
  , moduleDeclarationExists
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
  outputOutputable name
  string " where"
outputModuleDeclaration HsModule { hsmodName = Just name
                                 , hsmodExports = Just (L _ [])
                                 } = do
  string "module "
  outputOutputable name
  newline
  indentedBlock $ do
    string "("
    newline
    string ") where"
outputModuleDeclaration HsModule { hsmodName = Just name
                                 , hsmodExports = Just (L _ (x:xs))
                                 } = do
  string "module "
  outputOutputable name
  newline
  indentedBlock $ do
    string "( "
    outputOutputable x
    newline
    forM_ xs $ \e -> do
      string ", "
      outputOutputable e
      newline
    string ") where"

moduleDeclarationExists :: HsModule -> Bool
moduleDeclarationExists HsModule {hsmodName = Nothing} = False
moduleDeclarationExists _                              = True
