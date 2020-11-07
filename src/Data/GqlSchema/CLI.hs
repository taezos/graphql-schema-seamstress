{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
module Data.GqlSchema.CLI where

import           Import

import           Options.Applicative

import           Data.GqlSchema.Feedback

class Monad m => ManageCLI m where
  parseCliCommand :: m Command
  interpretCliCommand :: Command -> m [ StitchVomitResponse ]

data Command
  = StitchVomit StitchVomitInput
  deriving ( Eq, Show )

data StitchVomitInput  = StitchVomitInput
  { stitchVomitInputSchemaDirs :: FilePath
  , stitchVomitInputOutput     :: FilePath
  } deriving ( Eq, Show )

parseCommand :: Parser Command
parseCommand = StitchVomit
  <$> ( StitchVomitInput
        <$> strOption ( long "schema-src-dir" <> help "Schema source directory." )
        <*> strOption ( long "vomit-output" <> help "Stitched result of all the schemas." )
      )

showHelpOnErrorExecParser :: ParserInfo a -> IO a
showHelpOnErrorExecParser = customExecParser ( prefs showHelpOnError )

stitchVomitDesc :: String
stitchVomitDesc = "Graphql Stitch Vomit is a tool that stitches all graphql schema"

stitcVomitHeader :: String
stitcVomitHeader = "Graphql Stitch Vomit: A tool to stitch and vomit your schemas"
