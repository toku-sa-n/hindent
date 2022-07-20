{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           Data.Generics.Schemes
import           Data.Maybe                                          (mapMaybe)
import           GHC.Driver.Ppr                                      (showPpr)
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Utils.Outputable                                (Outputable)
import           HIndent.Pretty.Combinators
import           HIndent.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config
import           Text.Regex.TDFA

-- | Pretty print including comments.
pretty :: HsModule -> Printer ()
pretty m = do
  printPragmasToPrinter m
  printOutputableToPrinter m

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

printPragmasToPrinter :: HsModule -> Printer ()
printPragmasToPrinter m =
  case collectPragmas m of
    [] -> return ()
    xs -> do
      mapM_ string xs
      newline
      newline

collectPragmas :: HsModule -> [String]
collectPragmas =
  mapMaybe unwrapComment . filter isPragma . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True
    unwrapComment (EpaBlockComment c) = Just c
    unwrapComment _                   = Nothing

isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ ("{-# +LANGUAGE +[a-zA-Z]+ +#-}" :: String)
isPragma _                   = False

printOutputableToPrinter :: Outputable a => a -> Printer ()
printOutputableToPrinter = string . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags
