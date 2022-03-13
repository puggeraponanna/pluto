module Pluto.Logger
  ( logError,
    logWarning,
    logInfo,
  )
where

import           Control.Monad.Cont
import           System.Console.Pretty
import           System.Log.FastLogger

data LoggingType = Error | Info | Warning
  deriving (Show)

logToStdOut ::
  MonadIO m =>
  LoggingType ->
  Color ->
  String ->
  m ()
logToStdOut logType clr msg = liftIO $ do
  getTimeStamp <- newTimeCache "%Y-%m-%d %H:%M:%S"
  withTimedFastLogger getTimeStamp (LogStdout defaultBufSize) $
    \logger -> logger . logFormat logType clr $ msg <> "\n"

logFormat ::
  LoggingType ->
  Color ->
  String ->
  FormattedTime ->
  LogStr
logFormat logType clr msg time =
  toLogStr $ color clr ("\n[" <> show logType <> "] ") <> show time <> ": " <> msg <> "\n"

logError :: MonadIO m => String -> m ()
logError = logToStdOut Error Red

logWarning :: MonadIO m => String -> m ()
logWarning = logToStdOut Warning Yellow

logInfo :: MonadIO m => String -> m ()
logInfo = logToStdOut Info Green
