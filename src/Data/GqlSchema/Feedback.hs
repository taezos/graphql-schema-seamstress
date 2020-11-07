module Data.GqlSchema.Feedback where

import           Import

data StitchVomitError
  = InvalidDirectory
  | MissingOutputPath
  | MissingSourceDirectoryPath
  deriving ( Eq, Show )

type Filename = Text

data StitchVomitResponse
  = VomitedGQLFile Filename
  deriving ( Eq, Show )
