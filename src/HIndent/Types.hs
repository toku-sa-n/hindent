{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}

-- | All types.
module HIndent.Types
  ( Printer(..)
  , PrintState(..)
  , Config(..)
  , Inside(..)
  , readExtension
  , defaultConfig
  ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Strict (MonadState (..), StateT)
import           Control.Monad.Trans.Maybe
import           Data.ByteString.Builder
import           Data.Functor.Identity
import           Data.Int                   (Int64)
import           Data.Maybe
import           Data.Set
import           Data.Yaml                  (FromJSON (..))
import qualified Data.Yaml                  as Y
import           Language.Haskell.Extension (Extension (UnknownExtension),
                                             classifyExtension)

-- | A pretty printing monad.
newtype Printer a =
  Printer
    { runPrinter :: StateT PrintState (MaybeT Identity) a
    }
  deriving ( Applicative
           , Monad
           , Functor
           , MonadState PrintState
           , MonadPlus
           , Alternative
           )

-- TODO: Rename `Inside` to `Condition`.
data Inside
  = InsideDeclSig
  | InsideVerticalList
  | InsideVerticalFunctionSignature
  | InsideLambda
  | InsideMultiwayIf
  | InsideVerticalHsApp
  | InsideInstDecl
  | InsideCase
  | InsideConPat
  deriving (Eq, Ord)

-- | The state of the pretty printer.
data PrintState =
  PrintState
    { psIndentLevel  :: !Int64
    -- ^ Current indentation level, i.e. every time there's a
    -- new-line, output this many spaces.
    , psOutput       :: !Builder
    -- ^ The current output bytestring builder.
    , psNewline      :: !Bool
    -- ^ Just outputted a newline?
    , psColumn       :: !Int64
    -- ^ Current column.
    , psLine         :: !Int64
    -- ^ Current line number.
    , psConfig       :: !Config
    -- ^ Configuration of max colums and indentation style.
    , psFitOnOneLine :: !Bool
    -- ^ Bail out if we need to print beyond the current line or
    -- the maximum column.
    , psEolComment   :: !Bool
    , psInside       :: Set Inside
    -- ^ Denotes if the current position is inside some expression,
    -- signature, etc.
    -- TODO: Rename `psInside` to `psCondition`.
    }

-- | Configurations shared among the different styles. Styles may pay
-- attention to or completely disregard this configuration.
data Config =
  Config
    { configMaxColumns      :: !Int64 -- ^ Maximum columns to fit code into ideally.
    , configIndentSpaces    :: !Int64 -- ^ How many spaces to indent?
    , configTrailingNewline :: !Bool -- ^ End with a newline.
    , configSortImports     :: !Bool -- ^ Sort imports in groups.
    , configLineBreaks      :: [String] -- ^ Break line when meets these operators.
    , configExtensions      :: [Extension]
      -- ^ Extra language extensions enabled by default.
    }
-- | Parse an extension.
#if __GLASGOW_HASKELL__ >= 808
readExtension :: (Monad m, MonadFail m) => String -> m Extension
#else
readExtension :: Monad m => String -> m Extension
#endif
readExtension x =
  case classifyExtension x -- Foo
        of
    UnknownExtension _ -> fail ("Unknown extension: " ++ x)
    x'                 -> return x'

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config <$>
    fmap (fromMaybe (configMaxColumns defaultConfig)) (v Y..:? "line-length") <*>
    fmap
      (fromMaybe (configIndentSpaces defaultConfig))
      (v Y..:? "indent-size" <|> v Y..:? "tab-size") <*>
    fmap
      (fromMaybe (configTrailingNewline defaultConfig))
      (v Y..:? "force-trailing-newline") <*>
    fmap (fromMaybe (configSortImports defaultConfig)) (v Y..:? "sort-imports") <*>
    fmap (fromMaybe (configLineBreaks defaultConfig)) (v Y..:? "line-breaks") <*>
    (traverse readExtension . fromMaybe [] =<< v Y..:? "extensions")
  parseJSON _ = fail "Expected Object for Config value"

-- | Default style configuration.
defaultConfig :: Config
defaultConfig =
  Config
    { configMaxColumns = 80
    , configIndentSpaces = 2
    , configTrailingNewline = True
    , configSortImports = True
    , configLineBreaks = []
    , configExtensions = []
    }
