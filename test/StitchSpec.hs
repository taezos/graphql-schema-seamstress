{-# LANGUAGE TypeApplications #-}
module StitchSpec where

import           TestImport

import           Data.GqlSchema.Stitch

spec :: Spec
spec = do
  describe "Stitch" $ do
    it "should stitch query" $ do
      gqls <- stitchQuery "test/test-schema"
      shouldBe ( isRight gqls ) True
