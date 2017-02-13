{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Control.Monad.Freer.Logger
  (
    -- * Re-exports from monad-logger
    LogLevel(..)
  , LogSource

    -- * Re-export from fast-logger
  , LogStr
  , ToLogStr(..)
  , fromLogStr

    -- * Re-exports from freer
  , module Control.Monad.Freer

    -- * Logger effect
  , Logger
  , log
  , runLogger
  , runStderrLogging
  , runStdoutLogging

    -- * TH logging
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logOther

    -- * TH logging of showable values
  , logDebugSH
  , logInfoSH
  , logWarnSH
  , logErrorSH
  , logOtherSH

    -- * TH logging with source
  , logDebugS
  , logInfoS
  , logWarnS
  , logErrorS
  , logOtherS

    -- * TH util
  , liftLoc

    -- * Utilities for defining your own loggers
  , defaultLogStr
  , Loc (..)
  ) where

--------------------------------------------------------------------------------
import           Control.Monad.Freer
import           Control.Monad.Freer.Internal
import           Control.Monad.Logger         hiding (logDebug, logError,
                                               logInfo, logOther, logWarn)
import qualified Data.ByteString.Char8        as BS8
import qualified Data.Text                    as T
import           Language.Haskell.TH.Syntax   (Exp, Lift (lift), Loc (..), Q,
                                               qLocation)
import           Prelude                      hiding (log)
import           System.IO                    (Handle, stderr, stdout)
import           System.Log.FastLogger        (fromLogStr)
--------------------------------------------------------------------------------

data Logger v where
  Log :: Loc -> LogSource -> LogLevel -> LogStr -> Logger ()

log :: Member Logger r => Loc -> LogSource -> LogLevel -> LogStr -> Eff r ()
log loc src lvl str = send (Log loc src lvl str)

logTH :: LogLevel -> Q Exp
logTH lvl = [| log $(qLocation >>= liftLoc) (T.pack "") $(lift lvl) . (toLogStr :: T.Text -> LogStr) |]

-- | Generates a function that takes a 'Text' and logs a 'LevelDebug' message. Usage:
--
-- > $(logDebug) "This is a debug log message"
logDebug :: Q Exp
logDebug = logTH LevelDebug

-- | See 'logDebug'
logInfo :: Q Exp
logInfo = logTH LevelInfo

-- | See 'logDebug'
logWarn :: Q Exp
logWarn = logTH LevelWarn

-- | See 'logDebug'
logError :: Q Exp
logError = logTH LevelError

-- | Generates a function that takes a 'Text' and logs a 'LevelOther' message. Usage:
--
-- > $(logOther "My new level") "This is a log message"
logOther :: T.Text -> Q Exp
logOther = logTH . LevelOther

runLogger :: Member m r => (Loc -> LogSource -> LogLevel -> LogStr -> m ()) -> Eff (Logger ': r) w -> Eff r w
runLogger _      (Val v) = return v
runLogger log_fn (E u q) =
    case decomp u of
      Right (Log loc src lvl str) -> send (log_fn loc src lvl str) >> runLogger log_fn (qApp q ())
      Left  u'                    -> E u' (tsingleton (runLogger log_fn . qApp q))

runStderrLogging :: Member IO r => Eff (Logger ': r) w -> Eff r w
runStderrLogging = runLogger (defaultOutput stderr)

runStdoutLogging :: Member IO r => Eff (Logger ': r) w -> Eff r w
runStdoutLogging = runLogger (defaultOutput stdout)

--------------------------------------------------------------------------------

-- The code below is adapted from monad-logger

defaultOutput :: Handle -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
defaultOutput h loc src lvl msg = BS8.hPutStr h (defaultLogStrBS loc src lvl msg)

defaultLogStrBS :: Loc -> LogSource -> LogLevel -> LogStr -> BS8.ByteString
defaultLogStrBS loc src lvl msg = fromLogStr (defaultLogStr loc src lvl msg)
