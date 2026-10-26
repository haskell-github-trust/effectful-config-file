-- | Configuration decoding errors.
module Data.Error.Config (
  ConfigError (..),
  ConfigNotFound (..),
) where

import Data.List (intercalate)
import Data.Text (Text)
import Toml (DecodeError, prettyDecodeError)

-- | Errors raised while decoding a TOML configuration.
--
-- @since 0.1.0.0
newtype ConfigError = ConfigError
  { _ce_errors :: [DecodeError]
  }

instance Eq ConfigError where
  (ConfigError a) == (ConfigError b) =
    concatMap prettyDecodeError a == concatMap prettyDecodeError b

instance Show ConfigError where
  show (ConfigError es) = "config error: " <> intercalate "; " (map prettyDecodeError es)

-- | A configuration value whose @env://@ or @file://@ source is absent.
--
-- @since 0.1.2.0
newtype ConfigNotFound = ConfigNotFound Text
  deriving (Eq, Show)
