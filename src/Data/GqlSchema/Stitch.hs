module Data.GqlSchema.Stitch where

import           Import

import           Language.GraphQL.AST.Parser
import           Language.GraphQL.AST.Document

import qualified Data.ByteString.Char8       as BS

import qualified Data.Text.Encoding as TE

import Text.Megaparsec

readDocument :: FilePath -> IO ( Either ( ParseErrorBundle Text Void ) Document )
readDocument fp = pure . parseDocument fp . TE.decodeUtf8 =<< BS.readFile fp

parseDocument
  :: FilePath
  -> Text
  -> Either ( ParseErrorBundle Text Void ) Document
parseDocument fileName s = parse document fileName s
