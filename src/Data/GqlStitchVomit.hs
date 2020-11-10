{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Data.GqlStitchVomit where

-- graphql-stitch-vomit
import           Data.GqlSchema.CLI
import           Data.GqlSchema.Feedback
import           Data.GqlSchema.Logging
import           Data.GqlSchema.Stitch
import           Import

-- optparse-applicative
import           Options.Applicative

-- safe-exceptions
import           Control.Exception.Safe

-- text
import qualified Data.Text               as T

-- morpheus-graphql-core
import           Data.Morpheus.Types.Internal.AST

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
  interpretCliCommand comm@( StitchVomit StitchVomitInput{..} ) = do
    schemaRes <- readSchemas stitchVomitInputSchemaDirs
    handleSchemaReadResult schemaRes comm

  parseCliCommand = liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc stitchVomitDesc <> header stitcVomitHeader )
    )

instance ManageQuery AppM where
  stitchQuery = stitchQueryImpl
  readSchemas = readSchemasImpl
  vomitQuery = vomitQueryImpl

handleSchemaReadResult
  :: ( MonadIO m, ManageQuery m )
  => Either StitchVomitError Schemas
  -> Command
  -> m ()
handleSchemaReadResult schemaRes comm = case schemaRes of
  Left err -> logError $ show err
  Right schemas -> do
    stitchedSchema <- stitchQuery schemas
    handleStitchedSchema stitchedSchema comm

handleStitchedSchema
  :: ( MonadIO m, ManageQuery m )
  => Either StitchVomitError ( Schema VALID )
  -> Command -> m ()
handleStitchedSchema stitchedSchema ( StitchVomit StitchVomitInput{..} ) =
  case stitchedSchema of
    Left err -> logInfo $ show err
    Right schema -> either
      ( logError . show )
      (\fp -> logInfo $ "Vomitted graphql file to " <> T.pack fp )
      =<< vomitQuery stitchVomitInputOutput schema
