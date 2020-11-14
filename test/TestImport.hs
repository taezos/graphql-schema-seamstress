{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TestImport
  ( module X
  , TestAppM(..)
  , ManageQuery(..)
  , schemaStitchResult
  ) where

import           Data.Morpheus.Types.Internal.AST

import           Data.GqlSchema.Feedback
import           Data.GqlSchema.Stitch

import           Relude                           as X

import           Test.Hspec                       as X

import           Control.Monad.Except

import qualified Data.HashMap.Strict              as HS

newtype TestAppM m a
  = TestAppM
  { unTestAppM :: ( ExceptT StitchVomitError ( StateT (  HashMap Text ( Schema VALID ) ) m ) a )
  } deriving
  ( Functor
  , Applicative
  , Monad
  , MonadError StitchVomitError
  , MonadState (  HashMap Text ( Schema VALID ) )
  )

instance Monad m => ManageQuery ( TestAppM m ) where
  stitchSchemas = stitchSchemasImpl
  readSchemas = readSchemasTestImpl
  vomitQuery = vomitQueryTestImpl

readSchemasTestImpl
  :: ( MonadState (  HashMap Text ( Schema VALID ) ) m, MonadError StitchVomitError m )
  => FilePath
  -> m Schemas
readSchemasTestImpl _ = liftEither $ Right schemaBS

vomitQueryTestImpl
  :: ( MonadState (  HashMap Text ( Schema VALID ) ) m , MonadError StitchVomitError m )
  => FilePath
  -> Schema VALID
  -> m FilePath
vomitQueryTestImpl fp schema = do
  put ( HS.singleton "schema" schema )
  pure fp

schemaBS :: [ ByteString ]
schemaBS =
  ["type Query {\n  getPerson(name: String): Person\n}\n\ntype Mutation {\n  setPersonName(name: String): Person\n}\n\ntype Person {\n  firstName: String\n  lastName: String\n}\n","type Subscription {\n    postAdded: Post\n}\n\ntype Query {\n    posts: [Post]\n}\n\ntype Mutation {\n    addPost(author: String, comment: String): Post\n}\n\ntype Post {\n    author: String\n    comment: String\n}\n","type Query {\n  dog: String\n}\n\ntype Mutation {\n  setDogName(name: String): Dog\n}\n\ntype Dog {\n  name: String\n}\n"
  ]

schemaStitchResult  :: Maybe ByteString
schemaStitchResult = Just
  "type Person {\n  firstName: String\n  lastName: String\n}\n\ntype Post {\n  author: String\n  comment: String\n}\n\ntype Dog {\n  name: String\n}\n\ntype Query {\n  dog: String\n  posts: [Post]\n  getPerson(name: String): Person\n}\n\ntype Mutation {\n  addPost(author: String, comment: String): Post\n  setDogName(name: String): Dog\n  setPersonName(name: String): Person\n}\n\ntype Subscription {\n  postAdded: Post\n}\n\nschema {\n  query: Query\n  mutation: Mutation\n  subscription: Subscription\n}\n"
