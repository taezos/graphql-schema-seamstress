{-# LANGUAGE TypeApplications #-}
module StitchSpec where

import           TestImport

import           Data.GqlSchema.Stitch

spec :: Spec
spec = do
  describe "Stitch" $ do
    it "should stitch query" $ do
      res <- runExceptT $ do
        schemaRes <- readSchemasImpl "test/test-schema"
        parseRes <- pure $ parseDocument <$> schemaRes
        pure ( schemaRes, parseRes, updateSchema parseRes )
      shouldBe ( isRight res ) True
