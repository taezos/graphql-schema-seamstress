{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Relude

import           Data.GqlSchema.Stitch

main :: IO ()
main = do
  gqls <- stitchQuery "test/test-schema"
  res <- vomitQuery "test/new-schema.gql" gqls
  case res of
    Left err -> print err
    Right _ -> print "success"
