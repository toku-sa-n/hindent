{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Pretty printing.
module HIndent.Pretty
  ( pretty
  ) where

import           Control.Monad.State.Strict                          hiding
                                                                     (state)
import qualified Data.ByteString.Builder                             as S
import           Data.Generics.Schemes
import           GHC.Driver.Ppr                                      (showPpr)
import           GHC.Driver.Session
import           GHC.Hs
import           GHC.Utils.Outputable                                (Outputable)
import           HIndent.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config
import           Text.Regex.TDFA

class PrettyPrint a where
  prettyPrintToPrinter :: a -> Printer ()

-- | Pretty print including comments.
pretty :: HsModule -> Printer ()
pretty = prettyPrintToPrinter

write :: String -> Printer ()
write x = do
  eol <- gets psEolComment
  hardFail <- gets psFitOnOneLine
  let addingNewline = eol && x /= "\n"
  when addingNewline newline
  state <- get
  let writingNewline = x == "\n"
      out :: String
      out =
        if psNewline state && not writingNewline
          then replicate (fromIntegral (psIndentLevel state)) ' ' <> x
          else x
      psColumn' =
        if additionalLines > 0
          then fromIntegral (length (concat (take 1 (reverse srclines))))
          else psColumn state + fromIntegral (length out)
  when
    hardFail
    (guard
       (additionalLines == 0 && psColumn' <= configMaxColumns (psConfig state)))
  modify
    (\s ->
       s
         { psOutput = psOutput state <> S.stringUtf8 out
         , psNewline = False
         , psLine = psLine state + fromIntegral additionalLines
         , psEolComment = False
         , psColumn = psColumn'
         })
  where
    srclines = lines x
    additionalLines = length (filter (== '\n') x)

string :: String -> Printer ()
string = write

newline :: Printer ()
newline = do
  write "\n"
  modify (\s -> s {psNewline = True})

dynFlags :: DynFlags
dynFlags = defaultDynFlags fakeSettings fakeLlvmConfig

instance PrettyPrint HsModule where
  prettyPrintToPrinter m = do
    printPragmasToPrinter m
    printOutputableToPrinter m

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
  map unwrapComment . filter isPragma . listify matchToComment . hsmodAnn
  where
    matchToComment :: EpaCommentTok -> Bool
    matchToComment = const True
    unwrapComment (EpaBlockComment c) = c
    unwrapComment _                   = undefined

isPragma :: EpaCommentTok -> Bool
isPragma (EpaBlockComment c) = c =~ ("{-# +LANGUAGE +[a-zA-Z]+ +#-}" :: String)
isPragma _                   = False

printOutputableToPrinter :: Outputable a => a -> Printer ()
printOutputableToPrinter = string . showOutputable

showOutputable :: Outputable a => a -> String
showOutputable = showPpr dynFlags
