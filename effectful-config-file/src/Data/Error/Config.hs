module Data.Error.Config (
  ConfigError (..),
) where

import Data.List (intercalate)
import Toml (DecodeError, prettyDecodeError)

newtype ConfigError = ConfigError
  { _ce_errors :: [DecodeError]
  }

instance Eq ConfigError where
  (ConfigError a) == (ConfigError b) =
    concatMap prettyDecodeError a == concatMap prettyDecodeError b

instance Show ConfigError where
  show (ConfigError es) = "config error: " <> intercalate "; " (map prettyDecodeError es)
