{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Effectful.ConfigSpec (
  spec,
) where

import Control.Concurrent.MVar (newMVar, readMVar)
import Control.Exception (IOException, try)
import Control.Lens (makeLenses, (^.))
import Data.Bifunctor (Bifunctor (..), first)
import Data.Config (ConfigValue (..), withSection)
import Data.Error.Config (ConfigError (..), ConfigNotFound (..))
import Data.Text (Text)
import qualified Data.Text as T (isInfixOf, pack, unlines, unpack)
import Effectful (runEff)
import Effectful.Config (getConfigValue, readConfig, readConfig', requireConfigValue)
import Effectful.Config.Log (runLog)
import Effectful.Config.Secrets (SecretsError, runSecretsError)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import OpenTelemetry.Exporter.InMemory.LogRecord (
  getExportedLogRecords,
  inMemoryLogRecordExporter,
 )
import OpenTelemetry.Instrumentation.MonadLogger (makeOTelLogCallback)
import OpenTelemetry.Internal.Common.Types (AnyValue (TextValue))
import OpenTelemetry.Internal.Log.Types (logRecordBody)
import OpenTelemetry.Log.Core (
  createLoggerProvider,
  emptyLoggerProviderOptions,
  forceFlushLoggerProvider,
  makeLogger,
  readLogRecord,
 )
import OpenTelemetry.Processor.Simple.LogRecord (
  SimpleLogRecordProcessorConfig (..),
  simpleLogRecordProcessor,
 )
import OpenTelemetry.Trace.Core (instrumentationLibrary)
import System.Directory (removeFile)
import System.Environment (setEnv)
import System.IO (hClose, hPutStr)
import System.IO.Temp (withSystemTempFile)
import System.Timeout (timeout)
import Test.Hspec (
  Spec,
  describe,
  expectationFailure,
  it,
  runIO,
  shouldBe,
  shouldSatisfy,
 )
import Toml (decode', prettyDecodeError)
import Toml.Schema (
  FromValue (..),
  Result (..),
  reqKey,
 )

data TestConfig = TestConfig
  { _test_a :: ConfigValue Int
  , _test_b :: ConfigValue String
  }
  deriving (Eq, Show)

instance FromValue TestConfig where
  fromValue =
    withSection "test" $
      TestConfig
        <$> reqKey "a"
        <*> reqKey "b"

makeLenses ''TestConfig

newtype FileConfig = FileConfig
  { _file_val :: ConfigValue String
  }
  deriving (Eq, Show)

makeLenses ''FileConfig

newtype TextConfig = TextConfig (ConfigValue Text)
  deriving (Eq, Show)

instance FromValue TextConfig where
  fromValue = withSection "text" $ TextConfig <$> reqKey "v"

instance Functor (Result e) where
  fmap _ (Failure e) = Failure e
  fmap f (Success e a) = Success e (f a)

instance Bifunctor Result where
  bimap f _ (Failure e) = Failure (fmap f e)
  bimap f g (Success e a) = Success (fmap f e) (g a)

-- | No-op logger for tests that don't care about log output.
silentLog :: a -> b -> c -> d -> IO ()
silentLog _ _ _ _ = pure ()

spec :: Spec
spec = do
  describe "getConfigValue" $ do
    let cfg = TestConfig (Env "test") (Literal "test")
    result <- runIO $ do
      p <- do
        setEnv "test" "42"
        newMVar cfg

      runEff
        . runErrorNoCallStack @SecretsError
        . runSecretsError
        . runLog silentLog
        . runReader p
        $ (,) <$> getConfigValue test_a <*> getConfigValue test_b

    it "fetch" $ do
      result `shouldBe` Right (42, "test")

  describe "Text config value" $ do
    it "parses literal and env" $ do
      readConfig "[text]\nv = \"test\"" `shouldBe` Right (TextConfig (Literal "test"))
      readConfig "[text]\nv = \"env://X\"" `shouldBe` Right (TextConfig (Env "X"))

  describe "readConfig" $ do
    let result = TestConfig (Literal 42) (Literal "test")

    it "parses without errors" $ do
      let inp =
            T.unlines
              [ "[test]"
              , "a = 42"
              , "b = \"test\""
              ]

      readConfig inp `shouldBe` Right result

    it "invalid schema" $ do
      first (fmap prettyDecodeError . _ce_errors) (readConfig @TestConfig "")
        `shouldBe` Left ["1:1: missing key: test in <top-level>"]

    it "unused key" $ do
      let inp =
            T.unlines
              [ "[test]"
              , "a = 42"
              , "b = \"test\""
              , "c = 0"
              ]

      first prettyDecodeError (decode' inp)
        `shouldBe` Success ["4:1: unexpected key: c in test"] result

  describe "getConfigValue file://" $ do
    it "reads the file contents" $ withSystemTempFile "effectful-config" $ \path h -> do
      hPutStr h "hello"
      hClose h

      p <- newMVar (FileConfig (File path))
      result <-
        runEff
          . runErrorNoCallStack @SecretsError
          . runSecretsError
          . runLog silentLog
          . runReader p
          $ getConfigValue file_val

      result `shouldBe` Right "hello"

    it "memoizes the fetched value" $ withSystemTempFile "effectful-config" $ \path h -> do
      hPutStr h "hello"
      hClose h

      p <- newMVar (FileConfig (File path))
      _ <-
        runEff
          . runErrorNoCallStack @SecretsError
          . runSecretsError
          . runLog silentLog
          . runReader p
          $ getConfigValue file_val

      cfg <- readMVar p
      (cfg ^. file_val) `shouldBe` Literal "hello"

      removeFile path

      result2 <-
        runEff
          . runErrorNoCallStack @SecretsError
          . runSecretsError
          . runLog silentLog
          . runReader p
          $ getConfigValue file_val

      result2 `shouldBe` Right "hello"

    it "restores the MVar when fetch throws" $ do
      p <- newMVar (FileConfig (File "/nonexistent"))
      result <-
        try @IOException
          $ runEff
            . runErrorNoCallStack @SecretsError
            . runSecretsError
            . runLog silentLog
            . runReader p
          $ getConfigValue file_val

      case result of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected an IOException"

      cfg <- timeout 1000000 (readMVar p)
      cfg `shouldBe` Just (FileConfig (File "/nonexistent"))

  describe "requireConfigValue" $ do
    it "throws ConfigNotFound on a missing file" $ do
      p <- newMVar (FileConfig (File "/nonexistent"))
      result <-
        runEff
          . runErrorNoCallStack @ConfigNotFound
          . runErrorNoCallStack @SecretsError
          . runSecretsError
          . runLog silentLog
          . runReader p
          $ requireConfigValue "file_val" file_val

      result `shouldBe` Left (ConfigNotFound "file_val")

  describe "readConfig' OTel logging" $ do
    it "emits parse warnings as OTel log records" $ withSystemTempFile "effectful-config" $ \path h -> do
      hPutStr h . T.unpack $
        T.unlines
          [ "[test]"
          , "a = 42"
          , "b = \"test\""
          , "c = 0"
          ]
      hClose h

      (exporter, ref) <- inMemoryLogRecordExporter
      proc' <- simpleLogRecordProcessor (SimpleLogRecordProcessorConfig exporter 5000000)
      lp <- createLoggerProvider [proc'] emptyLoggerProviderOptions
      let logger = makeLogger lp (instrumentationLibrary "effectful-config-file-test" "0.0.0")

      result <-
        runEff
          . runErrorNoCallStack @ConfigError
          . runLog (makeOTelLogCallback logger)
          $ readConfig' @TestConfig path

      case result of
        Left e -> expectationFailure (show e)
        Right _ -> pure ()

      _ <- forceFlushLoggerProvider lp Nothing
      records <- getExportedLogRecords ref
      bodies <- mapM readLogRecord records

      let texts = [t | ilr <- bodies, TextValue t <- [logRecordBody ilr]]
      texts `shouldSatisfy` any (T.isInfixOf (T.pack "unexpected key: c"))
