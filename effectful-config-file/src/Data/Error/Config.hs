module Data.Error.Config (
  ConfigError (..),
) where

import Data.List (intercalate)
import Toml (DecodeError, prettyDecodeError)

-- | @since 0.1.0.0
newtype ConfigError = ConfigError
  { _ce_errors :: [DecodeError]
  }

instance Eq ConfigError where
  (ConfigError a) == (ConfigError b) =
    concatMap prettyDecodeError a == concatMap prettyDecodeError b

instance Show ConfigError where
  show (ConfigError es) = "config error: " <> intercalate "; " (map prettyDecodeError es)
