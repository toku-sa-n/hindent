module HIndent.Pretty.ModuleDeclaration
  ( outputModuleDeclaration
  , moduleDeclarationExists
  ) where

import           GHC.Hs
import           GHC.Types.SrcLoc           (GenLocated (..))
import           HIndent.Pretty.Combinators
import           HIndent.Types

outputModuleDeclaration :: HsModule -> Printer ()
outputModuleDeclaration HsModule {hsmodName = Nothing} = return ()
outputModuleDeclaration HsModule {hsmodName = Just name, hsmodExports = Nothing} = do
  string "module "
  output name
  string " where"
outputModuleDeclaration HsModule { hsmodName = Just name
                                 , hsmodExports = Just (L _ xs)
                                 } = do
  string "module "
  output name
  newline
  indentedBlock $ do
    verticalTuple (fmap output xs)
    string " where"

moduleDeclarationExists :: HsModule -> Bool
moduleDeclarationExists HsModule {hsmodName = Nothing} = False
moduleDeclarationExists _                              = True
