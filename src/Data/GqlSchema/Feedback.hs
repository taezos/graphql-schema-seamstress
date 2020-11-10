module Data.GqlSchema.Feedback where

-- graphql-stitch-vomit
import           Import

data StitchVomitError = StitchVomitError
  { stitchVomitErrorMsg :: Text
  } deriving ( Eq, Show )

instance Exception StitchVomitError

type Filename = Text

data StitchVomitResponse
  = VomitedGQLFile Filename
  deriving ( Eq, Show )
