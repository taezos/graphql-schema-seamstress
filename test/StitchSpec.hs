{-# LANGUAGE TypeApplications #-}
module StitchSpec where

import           TestImport

import           Data.GqlSchema.Stitch

import qualified Data.HashMap.Strict as HS

spec :: Spec
spec = do
  describe "Stitch" $ do
    it "should parse schema" $ do
      res <- runExceptT $ do
        schemaRes <- readSchemasImpl "test/test-schema"
        parseRes <- pure $ parseDocument <$> schemaRes
        pure parseRes
      shouldBe ( isRight res ) True
    it "should stitch schemas" $ do
      res <- flip runStateT HS.empty $ runExceptT $ unTestAppM $ do
        schemas <- readSchemas "test-dir"
        stitchedSchema <- stitchSchemas schemas
        vomitQuery "output.gql" stitchedSchema
      shouldBe ( fst res ) ( Right "output.gql" )
      shouldBe ( renderSchema <$> ( HS.lookup "schema" $ snd res ) ) schemaStitchResult
