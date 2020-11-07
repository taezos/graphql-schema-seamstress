module Data.GqlSchema.Logging where

-- graphql-stitch-vomit
import           Import

import qualified System.Console.ANSI as ANSI

import qualified Data.Text           as T

data LogMessage = LogMessage
  { logMessageText   :: Text
  , logMessageHeader :: Text
  } deriving ( Eq, Show )

data Log = Log
  { logReason  :: Severity
  , logMessage :: LogMessage
  } deriving ( Eq, Show )

data Severity
  = Info
  | Error
  deriving ( Eq, Show )

mkLog :: Monad m => Severity -> Text -> m Log
mkLog reason msg = do
  pure $ Log
    { logReason = reason
    , logMessage = LogMessage
      { logMessageText = msg
      , logMessageHeader = mkHeader reason
      }
    }
  where
    mkHeader :: Severity -> Text
    mkHeader sev = case sev of
      Info  -> "[Info]: "
      Error -> "[Error]: "

logInfo :: MonadIO m => Text -> m ()
logInfo msg = terminalLog =<< mkLog Info msg

logError :: MonadIO m => Text -> m ()
logError msg = terminalLog =<< mkLog Error msg

terminalLog :: MonadIO m => Log -> m ()
terminalLog logDesc = do
  liftIO $ ANSI.setSGR [ ANSI.SetColor ANSI.Foreground ANSI.Dull ( severityToColor $ logReason logDesc ) ]
  putStr $ T.unpack ( logMessageHeader $ logMessage logDesc )
  liftIO $ ANSI.setSGR []
  putStrLn $ T.unpack ( logMessageText $ logMessage logDesc )
  where
    severityToColor :: Severity -> ANSI.Color
    severityToColor sev = case sev of
      Info  -> ANSI.Green
      Error -> ANSI.Red
