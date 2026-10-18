{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Data.Config (
  ConfigParse (..),
  ConfigFetch (..),
  ConfigValue (..),
  withSection,
) where

import Control.Applicative.Combinators (choice)
import Control.Monad (void)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T (pack, unpack)
import Data.Typeable (Typeable, typeOf)
import Data.Void (Void)
import Effectful (Eff, IOE, liftIO, type (:>))
import Effectful.Config.Log (Log, logDebug)
import Effectful.Config.Secrets (Secrets, getSecret)
import OpenTelemetry.Trace.Core (
  Span,
  addAttribute,
  defaultSpanArguments,
  getGlobalTracerProvider,
  inSpan',
  instrumentationLibrary,
  makeTracer,
  tracerOptions,
 )
import System.Directory (getHomeDirectory)
import System.Environment (getEnv)
import System.FilePath ((</>))
import Text.Megaparsec (
  Parsec,
  chunk,
  errorBundlePretty,
  runParser,
  takeRest,
  try,
 )
import Text.Printf (printf)
import Toml.Schema (
  FromValue (..),
  Matcher,
  ParseTable,
  parseTable,
  reqKeyOf,
  warnAt,
 )
import Toml.Semantics.Types (Value' (..))

class ConfigParse a where
  parse :: Text -> a

instance ConfigParse Int where
  parse = read . T.unpack

instance ConfigParse ByteString where
  parse = B.pack . T.unpack

instance ConfigParse String where
  parse = T.unpack

-- | Class to fetch values from different sources.
class ConfigFetch a where
  fetch
    :: IOE :> es
    => Log :> es
    => Secrets :> es
    => a
    -> Eff es a

-- | Base configuration value.
--
-- @since 0.1.0.0
data ConfigValue a
  = -- | Literal string-readable value.
    Literal a
  | -- | Value from an environment variable (man 7 environ).
    Env String
  | -- | Value from a file.
    File FilePath
  | -- | Value from a secretspec-managed secret.
    Secret Text
  deriving (Eq, Show)

instance Functor ConfigValue where
  fmap f (Literal a) = Literal (f a)
  fmap _ (Env e) = Env e
  fmap _ (File f) = File f
  fmap _ (Secret s) = Secret s

configValueParser :: Parsec Void String (ConfigValue a)
configValueParser = choice [env, file, secret]
 where
  makePrefix = (<> "://")
  prefix = void . try . chunk . makePrefix
  withPrefix p = prefix p >> takeRest
  env = Env <$> withPrefix "env"
  file = File <$> withPrefix "file"
  secret = Secret . T.pack <$> withPrefix "secretspec"

parseTextValue
  :: FromValue a
  => Value' l
  -> Matcher l (ConfigValue a)
parseTextValue v@(Text' _ t) =
  either
    (const (Literal <$> fromValue v))
    pure
    (runParser configValueParser "ConfigValue String" (T.unpack t))
parseTextValue _ = fail "must be text value"

instance {-# OVERLAPPING #-} FromValue (ConfigValue String) where
  fromValue = parseTextValue

instance (FromValue a, Typeable a) => FromValue (ConfigValue a) where
  fromValue v@(Text' l t) =
    either
      (\e -> warnAt l (errorBundlePretty e) >> fromValue v)
      pure
      (runParser configValueParser ("ConfigValue " <> show (typeOf @a undefined)) (T.unpack t))
  fromValue v = Literal <$> fromValue v

-- | Wraps a config value fetch in a @config.fetch@ span tagged with the
-- source kind and the (unexpanded) key. Never logs the fetched value.
withConfigSpan
  :: IOE :> es
  => Text
  -> Text
  -> (Span -> Eff es a)
  -> Eff es a
withConfigSpan source key act = do
  tp <- getGlobalTracerProvider
  let tracer = makeTracer tp (instrumentationLibrary "effectful-config-file" "0.1.0.0") tracerOptions
  inSpan' tracer "config.fetch" defaultSpanArguments $ \sp -> do
    addAttribute sp "config.source" source
    addAttribute sp "config.key" key
    act sp

instance ConfigParse a => ConfigFetch (ConfigValue a) where
  fetch (Env e) =
    withConfigSpan "env" (T.pack e) $ \_ -> do
      logDebug $ "fetching config value from env " <> T.pack e
      v <- liftIO (getEnv e)
      pure $ Literal (parse (T.pack v))
  fetch (File fp) =
    withConfigSpan "file" (T.pack fp) $ \_ -> do
      fp' <-
        if "~/" `isPrefixOf` fp
          then do
            home <- liftIO getHomeDirectory
            pure $ home </> drop 2 fp
          else pure fp

      logDebug $ "fetching config value from file " <> T.pack fp
      v <- liftIO (readFile fp')
      pure $ Literal (parse (T.pack v))
  fetch (Secret k) =
    withConfigSpan "secretspec" k $ \_ -> do
      logDebug $ "fetching config value from secretspec " <> k
      Literal . parse <$> getSecret k
  fetch v@(Literal _) = pure v

-- | @since 0.1.0.0
withSection
  :: Text
  -> ParseTable l a
  -> Value' l
  -> Matcher l a
withSection key parser (Table' lo to) =
  parseTable (reqKeyOf key parseInner) lo to
 where
  parseInner (Table' li ti) = parseTable parser li ti
  parseInner _ = fail $ printf "section [%s] must be a table" (T.unpack key)
withSection key _ _ =
  fail $ printf "section [%s] expected a top-level table" (T.unpack key)
