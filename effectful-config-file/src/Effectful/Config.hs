{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | Effect to read a TOML configuration and lazily resolve its values.
module Effectful.Config (
  Config,
  getConfig,
  getConfigValue,
  readConfig,
  readConfig',
  requireConfigValue,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.Exception (IOException)
import Control.Lens (ALens', cloneLens, (&), (.~), (^.))
import Data.Config (ConfigFetch (fetch), ConfigParse, ConfigValue (..))
import Data.Error.Config (ConfigError (..), ConfigNotFound (..))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (readFile)
import Effectful (Eff, IOE, liftIO, withSeqEffToIO, type (:>))
import Effectful.Config.Log (Log, logWarn)
import Effectful.Config.Secrets (Secrets)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (catch)
import Effectful.Reader.Static (Reader, ask)
import OpenTelemetry.Trace.Core (
  addAttribute,
  defaultSpanArguments,
  getGlobalTracerProvider,
  inSpan',
  instrumentationLibrary,
  makeTracer,
  tracerOptions,
 )
import Text.Printf (printf)
import Toml (Result (..), decode', prettyDecodeError)
import Toml.Schema.FromValue (FromValue)

-- | Effect giving access to a shared, mutable configuration of type @c@.
--
-- @since 0.1.0.0
type Config c = Reader (MVar c)

-- | Get the current configuration.
--
-- @since 0.1.0.0
getConfig
  :: IOE :> es
  => Config c :> es
  => Eff es c
getConfig = ask >>= liftIO . readMVar

-- | Fetch the value at the given lens from its source, caching the result
-- as a 'Literal' in the configuration for subsequent calls.
--
-- @since 0.1.0.0
getConfigValue
  :: IOE :> es
  => Config c :> es
  => Log :> es
  => Secrets :> es
  => ConfigParse a
  => Show a
  => ALens' c (ConfigValue a)
  -- ^ getter lens
  -> Eff es a
getConfigValue lens = do
  mvar <- ask
  withSeqEffToIO $ \runInIO ->
    modifyMVar mvar $ \cfg -> do
      v <- runInIO (fetch (cfg ^. cloneLens lens))
      case v of
        Literal b -> pure (cfg & cloneLens lens .~ v, b)
        _ ->
          error $
            unlines
              [ printf "tried to get raw config value from (%s)" (show v)
              , "this should be impossible and is most likely a bug in your ConfigFetch instance"
              ]

-- | 'getConfigValue' wrapped with 'ConfigNotFound' on missing file path
--
-- @since 0.1.2.0
requireConfigValue
  :: IOE :> es
  => Config c :> es
  => Log :> es
  => Secrets :> es
  => Error ConfigNotFound :> es
  => ConfigParse a
  => Show a
  => Text
  -- ^ display name of the value
  -> ALens' c (ConfigValue a)
  -- ^ getter lens
  -> Eff es a
requireConfigValue name lens =
  getConfigValue lens `catch` \(_ :: IOException) -> throwError (ConfigNotFound name)

-- | Decode a configuration from TOML text. Warnings are ignored.
--
-- @since 0.1.0.0
readConfig
  :: FromValue c
  => Text
  -- ^ contents to parse
  -> Either ConfigError c
readConfig =
  resultToEither . decode'
 where
  resultToEither (Failure es) = Left (ConfigError es)
  resultToEither (Success _ c) = Right c

-- | Read and decode a TOML configuration file, logging decoding warnings
-- and throwing t'ConfigError' on failure.
--
-- @since 0.1.0.0
readConfig'
  :: forall c es
   . IOE :> es
  => Error ConfigError :> es
  => Log :> es
  => FromValue c
  => FilePath
  -- ^ path to read
  -> Eff es c
readConfig' path = do
  tp <- getGlobalTracerProvider
  let tracer = makeTracer tp (instrumentationLibrary "effectful-config-file" "0.1.0.0") tracerOptions
  inSpan' tracer "config.read" defaultSpanArguments $ \sp -> do
    addAttribute sp "config.path" (T.pack path)
    processResult . decode' =<< liftIO (T.readFile path)
 where
  processResult (Failure es) = throwError (ConfigError es)
  processResult (Success [] c) = pure c
  processResult (Success es c) = do
    logWarn "configuration parsed with warnings:"
    mapM_ (logWarn . ("  " <>) . T.pack . prettyDecodeError) es
    pure c
