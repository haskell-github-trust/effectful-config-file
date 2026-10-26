{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Effect to resolve secrets referenced by configuration values.
module Effectful.Config.Secrets (
  Secrets (..),
  getSecret,
  SecretsError (..),
  runSecretsError,
) where

import Data.Text (Text)
import Effectful (Dispatch (Dynamic), DispatchOf, Eff, Effect, type (:>))
import Effectful.Dispatch.Dynamic (interpret, send)
import Effectful.Error.Static (Error, throwError)
import GHC.Stack (HasCallStack)

-- | Effect to look up secrets by key.
--
-- @since 0.1.0.0
data Secrets :: Effect where
  -- | Look up the secret with the given key.
  GetSecret :: Text -> Secrets m Text

type instance DispatchOf Secrets = Dynamic

-- | Look up the secret with the given key.
--
-- @since 0.1.0.0
getSecret
  :: HasCallStack
  => Secrets :> es
  => Text -> Eff es Text
getSecret = send . GetSecret

-- | Errors raised when a secret cannot be resolved.
--
-- @since 0.1.0.0
data SecretsError
  = -- | No secrets backend is configured to resolve the key.
    NoSecretsBackend Text
  | -- | The backend has no secret with this key.
    SecretNotFound Text
  deriving (Eq, Show)

-- | Run 'Secrets' without a backend: every lookup throws 'NoSecretsBackend'.
--
-- @since 0.1.0.0
runSecretsError
  :: Error SecretsError :> es
  => Eff (Secrets : es) a
  -> Eff es a
runSecretsError = interpret $ \_ (GetSecret key) -> throwError (NoSecretsBackend key)
