{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.GqlStitchVomit where

-- graphql-stitch-vomit
import           Data.GqlSchema.CLI
import           Data.GqlSchema.Logging
import           Data.GqlSchema.Stitch
import           Import

-- optparse-applicative
import           Options.Applicative

-- safe-exceptions
import           Control.Exception.Safe

import qualified Data.Text              as T

newtype AppM a
  = AppM
  { unAppM :: IO a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadThrow
  , MonadCatch
  )

runApp :: IO ()
runApp = do
  unAppM $ interpretCliCommand =<< parseCliCommand

instance ManageCLI AppM where
  interpretCliCommand ( StitchVomit StitchVomitInput{..} ) = do
    schemas <- readSchemas stitchVomitInputSchemaDirs
    stitchedSchema <- stitchQuery schemas
    case stitchedSchema of
      Left err -> logInfo $ show err
      Right schema -> either
        ( logError . show )
        (\fp -> logInfo $ "Vomitted graphql file to " <> T.pack fp )
        =<< vomitQuery stitchVomitInputOutput schema

  parseCliCommand = liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc stitchVomitDesc <> header stitcVomitHeader )
    )

instance ManageQuery AppM where
  stitchQuery = stitchQueryImpl
  readSchemas = readSchemasImpl
  vomitQuery = vomitQueryImpl
