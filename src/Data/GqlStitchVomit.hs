{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
module Data.GqlStitchVomit where

import           Data.GqlSchema.CLI
import           Data.GqlSchema.Logging
import           Import

import           Options.Applicative

import           Control.Exception.Safe

newtype AppM a
  = AppM
  { unAppM :: IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadThrow
  )

runApp :: IO ()
runApp = logInfo
  =<< unAppM
  $ interpretCliCommand
  =<< parseCliCommand

instance ManageCLI AppM where
  interpretCliCommand ( StitchVomit StitchVomitInput{..} ) = undefined
  parseCliCommand = liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc stitchVomitDesc <> header stitcVomitHeader )
    )
