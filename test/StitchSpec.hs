{-# LANGUAGE TypeApplications #-}
module StitchSpec where

import           TestImport

import           Data.GqlSchema.Stitch

import Data.Morpheus.Core

spec :: Spec
spec = do
  describe "Stitch" $ do
    it "should stitch query" $ do
      res <- runExceptT $ do
        schemaRes <- readSchemasImpl "test/test-schema"
        parseRes <- pure $ parseDocument <$> schemaRes
        print $ render <$> appendAllSchemaMutations parseRes
        pure ( schemaRes, parseRes, appendAllSchemaQueries parseRes )
      shouldBe @Int 1 2
      shouldBe ( isRight res ) True
