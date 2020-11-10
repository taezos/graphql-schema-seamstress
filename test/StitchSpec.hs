{-# LANGUAGE TypeApplications #-}
module StitchSpec where

import           TestImport

import           Data.GqlSchema.Stitch

spec :: Spec
spec = do
  describe "Stitch" $ do
    it "should stitch query" $ do
      schemaRes <- readSchemasImpl "test/test-schema"
      parseRes <- pure $ fmap parseDocument <$> schemaRes
      shouldBe ( isRight $ updateSchema =<< parseRes ) True
      shouldBe ( isRight schemaRes ) True
      shouldBe ( isRight parseRes ) True
