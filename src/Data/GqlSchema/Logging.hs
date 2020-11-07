module Data.GqlSchema.Logging where

import           Import

data LogMessage = LogMessage
  { _logMessageText   :: Text
  , _logMessageHeader :: Text
  } deriving ( Eq, Show )

data Log = Log
  { _logReason :: Severity
  , _logMessage :: LogMessage
  } deriving ( Eq, Show )

data Severity
  = Info
  | Error
  deriving ( Eq, Show )

mkLog :: Monad m => Severity -> Text -> m Log
mkLog reason msg = undefined

logInfo = undefined
