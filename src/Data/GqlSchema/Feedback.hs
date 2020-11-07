module Data.GqlSchema.Feedback where

-- graphql-stitch-vomit
import           Import

data StitchVomitError
  = InvalidDirectory
  | MissingOutputPath
  | MissingSourceDirectoryPath
  | StitchError Text
  | VomitError Text
  deriving ( Eq, Show )

instance Exception StitchVomitError

type Filename = Text

data StitchVomitResponse
  = VomitedGQLFile Filename
  deriving ( Eq, Show )
