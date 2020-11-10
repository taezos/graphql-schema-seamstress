module Data.GqlSchema.Feedback where

-- graphql-stitch-vomit
import           Import

data StitchVomitError
  = InvalidDirectory
  | InvalidOutputPath
  | InvalidSourceDirectoryPath
  | UnableToReadSchemas
  | SchemaParseError
  | SchemaUpdateError
  | StitchError Text
  | VomitError Text
  deriving ( Eq, Show )

instance Exception StitchVomitError

type Filename = Text

data StitchVomitResponse
  = VomitedGQLFile Filename
  deriving ( Eq, Show )
