{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

-- | Minimal logging effect used by the configuration library.
module Effectful.Config.Log (
  Log,
  runLog,
  logDebug,
  logWarn,
) where

import Control.Monad.Logger (
  Loc (..),
  LogLevel (..),
  LogSource,
  LogStr,
  ToLogStr (toLogStr),
 )
import Data.Text (Text)
import Effectful (Dispatch (Static), DispatchOf, Eff, Effect, IOE, type (:>))
import Effectful.Dispatch.Static (
  SideEffects (WithSideEffects),
  StaticRep,
  evalStaticRep,
  getStaticRep,
  unsafeEff_,
 )
import GHC.Stack (CallStack, HasCallStack, SrcLoc (..), callStack, getCallStack)

-- | Logging effect used by the library.
--
-- @since 0.1.0.0
data Log :: Effect

type instance DispatchOf Log = Static WithSideEffects

newtype instance StaticRep Log = Log (Loc -> LogSource -> LogLevel -> LogStr -> IO ())

-- | Run the t'Log' effect with a @monad-logger@ style logging function.
--
-- @since 0.1.0.0
runLog
  :: IOE :> es
  => (Loc -> LogSource -> LogLevel -> LogStr -> IO ())
  -> Eff (Log : es) a
  -> Eff es a
runLog = evalStaticRep . Log

logAt
  :: HasCallStack
  => Log :> es
  => LogLevel
  -> Text
  -> Eff es ()
logAt lvl msg = do
  Log cb <- getStaticRep
  unsafeEff_ $ cb (callSiteLoc callStack) "" lvl (toLogStr msg)

-- | Log a message at debug level.
--
-- @since 0.1.0.0
logDebug
  :: HasCallStack
  => Log :> es
  => Text
  -> Eff es ()
logDebug = logAt LevelDebug

-- | Log a message at warning level.
--
-- @since 0.1.0.0
logWarn
  :: HasCallStack
  => Log :> es
  => Text
  -> Eff es ()
logWarn = logAt LevelWarn

callSiteLoc :: CallStack -> Loc
callSiteLoc cs = case getCallStack cs of
  (_, loc) : _ ->
    Loc
      { loc_filename = srcLocFile loc
      , loc_package = srcLocPackage loc
      , loc_module = srcLocModule loc
      , loc_start = (srcLocStartLine loc, srcLocStartCol loc)
      , loc_end = (srcLocEndLine loc, srcLocEndCol loc)
      }
  [] -> Loc "<unknown>" "<unknown>" "<unknown>" (0, 0) (0, 0)
