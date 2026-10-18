{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.Config (
  Config,
  getConfig,
  getConfigValue,
  readConfig,
  readConfig',
) where

import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Control.Lens (ALens', cloneLens, (&), (.~), (^.))
import Data.Config (ConfigFetch (fetch), ConfigParse, ConfigValue (..))
import Data.Error.Config (ConfigError (..))
import Data.Text (Text)
import qualified Data.Text as T (pack)
import qualified Data.Text.IO as T (readFile)
import Effectful (Eff, IOE, liftIO, withSeqEffToIO, type (:>))
import Effectful.Config.Log (Log, logWarn)
import Effectful.Config.Secrets (Secrets)
import Effectful.Error.Static (Error, throwError)
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

type Config c = Reader (MVar c)

getConfig
  :: IOE :> es
  => Config c :> es
  => Eff es c
getConfig = ask >>= liftIO . readMVar

getConfigValue
  :: IOE :> es
  => Config c :> es
  => Log :> es
  => Secrets :> es
  => ConfigParse a
  => Show a
  => ALens' c (ConfigValue a)
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

readConfig
  :: FromValue c
  => Text
  -> Either ConfigError c
readConfig =
  resultToEither . decode'
 where
  resultToEither (Failure es) = Left (ConfigError es)
  resultToEither (Success _ c) = Right c

readConfig'
  :: forall c es
   . IOE :> es
  => Error ConfigError :> es
  => Log :> es
  => FromValue c
  => FilePath
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
