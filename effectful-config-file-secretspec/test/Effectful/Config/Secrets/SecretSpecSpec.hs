{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Effectful.Config.Secrets.SecretSpecSpec (
  spec,
) where

import Control.Concurrent.MVar (newMVar)
import Control.Lens (makeLenses)
import Data.Config (ConfigValue (..))
import qualified Data.Text as T (pack)
import Effectful (runEff)
import Effectful.Config (getConfigValue)
import Effectful.Config.Log (runLog)
import Effectful.Config.Secrets (SecretsError (..), getSecret)
import Effectful.Config.Secrets.SecretSpec (runSecretSpec)
import Effectful.Error.Static (runErrorNoCallStack)
import Effectful.Reader.Static (runReader)
import SecretSpec (Builder, withPath, withProvider, withReason)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe)

newtype SecretConfig = SecretConfig
  { _secret_val :: ConfigValue String
  }
  deriving (Eq, Show)

makeLenses ''SecretConfig

silentLog :: a -> b -> c -> d -> IO ()
silentLog _ _ _ _ = pure ()

withManifest :: (FilePath -> (Builder -> Builder) -> IO a) -> IO a
withManifest act = withSystemTempDirectory "secretspec" $ \dir -> do
  writeFile (dir </> ".env") "MY_SECRET=hunter2\n"
  writeFile
    (dir </> "secretspec.toml")
    ( unlines
        [ "[project]"
        , "name = \"test\""
        , "revision = \"1.0\""
        , ""
        , "[profiles.default]"
        , "MY_SECRET = { description = \"test\", required = true }"
        ]
    )
  act
    dir
    ( withPath (T.pack (dir </> "secretspec.toml"))
        . withProvider (T.pack ("dotenv://" <> (dir </> ".env")))
        . withReason "test"
    )

spec :: Spec
spec = do
  describe "runSecretSpec" $ do
    it "resolves a declared secret" $ withManifest $ \_ cfg -> do
      result <-
        runEff
          . runErrorNoCallStack @SecretsError
          . runSecretSpec cfg
          $ getSecret "MY_SECRET"

      result `shouldBe` Right "hunter2"

    it "errors on an undeclared secret" $ withManifest $ \_ cfg -> do
      result <-
        runEff
          . runErrorNoCallStack @SecretsError
          . runSecretSpec cfg
          $ getSecret "NOPE"

      result `shouldBe` Left (SecretNotFound "NOPE")

  describe "getConfigValue" $ do
    it "resolves a Secret config value via secretspec" $ withManifest $ \_ cfg -> do
      p <- newMVar (SecretConfig (Secret "MY_SECRET"))
      result <-
        runEff
          . runErrorNoCallStack @SecretsError
          . runSecretSpec cfg
          . runLog silentLog
          . runReader p
          $ getConfigValue secret_val

      result `shouldBe` Right "hunter2"
