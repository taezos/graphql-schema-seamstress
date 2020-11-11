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

-- text
import qualified Data.Text               as T

newtype AppM m a
  = AppM
  { unAppM :: ( ExceptT StitchVomitError m ) a
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadIO
  , MonadError StitchVomitError
  )

runApp :: IO ()
runApp = do
  res <- runExceptT $ unAppM $ interpretCliCommand =<< parseCliCommand
  case res of
    Left err -> logError $ stitchVomitErrorMsg err
    Right fp -> logInfo $ "Vomitted graphql file to " <> T.pack fp

instance MonadIO m => ManageCLI ( AppM m ) where
  interpretCliCommand ( StitchVomit StitchVomitInput{..} ) = do
    schemas <- readSchemas stitchVomitInputSchemaDirs
    stitchedSchema <- stitchSchemas schemas
    vomitQuery stitchVomitInputOutput stitchedSchema

  parseCliCommand = liftIO $ showHelpOnErrorExecParser
    ( info ( helper <*> parseCommand )
      ( fullDesc <> progDesc stitchVomitDesc <> header stitcVomitHeader )
    )

instance MonadIO m => ManageQuery ( AppM m ) where
  stitchSchemas = stitchSchemasImpl
  readSchemas = readSchemasImpl
  vomitQuery = vomitQueryImpl

