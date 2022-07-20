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
import           Data.List
import           GHC.Driver.Ppr
import           GHC.Driver.Session
import           GHC.Hs
import           HIndent.Types
import           Language.Haskell.GhclibParserEx.GHC.Settings.Config
import           Prelude                                             hiding
                                                                     (exp)

-- | Pretty print including comments.
pretty :: HsModule -> Printer ()
pretty = string . showPpr dynFlags

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
          then (replicate (fromIntegral (psIndentLevel state)) ' ') <> x
          else x
      psColumn' =
        if additionalLines > 0
          then fromIntegral (length (concat (take 1 (reverse srclines))))
          else psColumn state + fromIntegral (length out)
  when
    hardFail
    (guard
       (additionalLines == 0 && (psColumn' <= configMaxColumns (psConfig state))))
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
