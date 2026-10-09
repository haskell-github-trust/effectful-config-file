{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

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

data Secrets :: Effect where
  GetSecret :: Text -> Secrets m Text

type instance DispatchOf Secrets = Dynamic

getSecret
  :: HasCallStack
  => Secrets :> es
  => Text -> Eff es Text
getSecret = send . GetSecret

data SecretsError
  = NoSecretsBackend Text
  | SecretNotFound Text
  deriving (Eq, Show)

runSecretsError
  :: Error SecretsError :> es
  => Eff (Secrets : es) a
  -> Eff es a
runSecretsError = interpret $ \_ (GetSecret key) -> throwError (NoSecretsBackend key)
