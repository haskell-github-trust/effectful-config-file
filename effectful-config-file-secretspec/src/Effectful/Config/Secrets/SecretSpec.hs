{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- | secretspec backend for the 'Effectful.Config.Secrets.Secrets' effect.
module Effectful.Config.Secrets.SecretSpec (
  runSecretSpec,
) where

import qualified Data.Map.Strict as Map
import Effectful (Eff, IOE, liftIO, type (:>))
import Effectful.Config.Secrets (Secrets (..), SecretsError (..))
import Effectful.Dispatch.Dynamic (interpret)
import Effectful.Error.Static (Error, throwError)
import Effectful.Exception (bracket)
import SecretSpec (Builder, builder, close, get, load, resolvedSecrets)

-- | Run 'Secrets' with secretspec. The function customizes the default
-- 'Builder'; secrets are loaded once for the whole computation and missing
-- keys throw 'SecretNotFound'.
--
-- @since 0.1.0.0
runSecretSpec
  :: IOE :> es
  => Error SecretsError :> es
  => (Builder -> Builder)
  -> Eff (Secrets : es) a
  -> Eff es a
runSecretSpec f act =
  bracket (liftIO (load (f builder))) (liftIO . close) $ \r ->
    interpret
      ( \_ -> \case
          GetSecret k ->
            maybe
              (throwError (SecretNotFound k))
              pure
              (Map.lookup k (resolvedSecrets r) >>= get)
      )
      act
