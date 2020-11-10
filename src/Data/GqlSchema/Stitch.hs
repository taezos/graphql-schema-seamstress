{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.GqlSchema.Stitch where

-- bytestring
import qualified Data.ByteString.Char8            as BS
import qualified Data.ByteString.Lazy.Char8       as BL

-- directory
import           System.Directory

-- filepath
import           System.FilePath.Posix

-- graphql-stitch-vomit
import           Data.GqlSchema.Feedback
import           Import

-- microlens
import           Lens.Micro

-- morpheus-graphql-core
import           Data.Morpheus.Core
import           Data.Morpheus.Ext.Map
import           Data.Morpheus.Ext.SafeHashMap
import           Data.Morpheus.Types.Internal.AST

-- safe-exceptions
import           Control.Exception.Safe

-- unordered-containers
import qualified Data.HashMap.Strict              as HS

-- text
import qualified Data.Text.Encoding               as TE

class Monad m => ManageQuery m where
  stitchQuery :: Schemas -> m ( Either StitchVomitError ( Schema VALID ) )
  readSchemas :: FilePath -> m ( Either StitchVomitError Schemas )
  vomitQuery :: FilePath -> Schema VALID -> m ( Either StitchVomitError FilePath )

type Schemas = [ ByteString ]

-- | Stitches all the type Queries in one file.
stitchQueryImpl
  :: Monad m
  => Schemas
  -> m ( Either StitchVomitError ( Schema VALID ) )
stitchQueryImpl schemas = pure
  . updateSchema
  $ parseDocument
  <$> schemas

-- | Reads all the schemas from the source directory.
readSchemasImpl
  :: ( MonadThrow m, MonadIO m )
  => FilePath
  -- ^ Source directory of schema.
  -> m ( Either StitchVomitError Schemas )
readSchemasImpl fp = liftIO $ do
  filePathRes <- tryAny $ listDirectory fp
  case filePathRes of
    Left _ -> pure $ Left InvalidSourceDirectoryPath
    Right filePaths -> fmap ( first ( const UnableToReadSchemas ) )
      $ tryAny
      $ sequence
      $ BS.readFile <$> ( mkFilePathsValid filePaths )
  where
    mkFilePathsValid :: [ FilePath ] -> [ FilePath ]
    mkFilePathsValid filePths =
      makeValid
      . (\fps -> fp <> ( pathSeparator : fps ))
      <$> filePths

-- | Vomits all the queries in one file
vomitQueryImpl
  :: ( MonadThrow m, MonadIO m )
  => FilePath
  -> Schema VALID
  -> m ( Either StitchVomitError FilePath )
vomitQueryImpl fp schema = do
  writeResponse <- writeSchema fp schema
  case writeResponse of
    Left _ -> pure $ Left InvalidOutputPath
    Right _ -> do
      absolutePath <- liftIO $ getCurrentDirectory
      pure $ Right $ absolutePath <> ( pathSeparator : fp )

-- |  Write updated schema to file.
writeSchema
  :: MonadIO m
  => FilePath
  -> Schema VALID
  -> m ( Either StitchVomitError FilePath )
writeSchema fp schema = do
  writeResponse <- liftIO $ tryAny $ BS.writeFile fp ( vomitMessage <> renderSchema schema )
  absolutePath <- liftIO getCurrentDirectory
  pure $ bimap
    ( const InvalidOutputPath  )
    ( const $ absolutePath <> ( pathSeparator : fp ) )
    writeResponse
  where
    vomitMessage :: ByteString
    vomitMessage = TE.encodeUtf8 $ unlines
      [ "\"\"\""
      , "DO NOT EDIT."
      , "THIS FILE IS MEANT TO BE OVERWRITTEN BY GRAPHQL-STITCH-VOMIT."
      , "YOU HAVE BEEN WARNED!!!"
      , "\"\"\""
      ]

parseDocument :: ByteString -> Either StitchVomitError ( Schema VALID )
parseDocument = first ( const SchemaParseError ) . parseDSL . BL.fromStrict

-- | Update schema with the accumulated queries from other graphql files.
updateSchema
  :: [ Either StitchVomitError ( Schema VALID ) ]
  -> Either StitchVomitError ( Schema VALID )
updateSchema parsedDocs = updateSchemaTypes parsedDocs $ first ( const SchemaUpdateError )
  $ updateSchemaEntries ( mkMapEntries parsedDocs )
  <$> getFirstQuery ( filterQuery parsedDocs )
  where
    mkMapEntries
      :: [ Either StitchVomitError ( Schema VALID ) ]
      -> HashMap FieldName ( Indexed FieldName ( FieldDefinition OUT VALID ) )
    mkMapEntries pds = HS.unions
      $ foldr (\a _ -> a ^. queryL . typeContentL . objectFieldsL . mapEntriesL ) HS.empty
      <$> pds

-- | Filter for type with matching type name. Takes only the matching type name
-- and filters out the rest.
filterSchema
  :: [ Either StitchVomitError ( Schema VALID ) ]
  -> Text
  -- ^ Type name to match.
  -> [ Either StitchVomitError ( Schema VALID ) ]
filterSchema parsedDocs typeNameTxt =
  foldr ( filterByTypeNameQuery typeNameTxt ) [] parsedDocs
  where
    filterByTypeNameQuery
      :: Text
      -> Either StitchVomitError ( Schema VALID )
      -> [ Either StitchVomitError ( Schema VALID ) ]
      -> [ Either StitchVomitError ( Schema VALID ) ]
    filterByTypeNameQuery typeName pd accum =  case pd of
      Left err -> Left err : accum
      Right schema ->
        if typeName == ( schema ^. queryL . typeNameL & readTypeName )
        then Right ( mkEmptyTypes schema ) : accum
        else accum

-- | Takes Query and filters out the rest.
filterQuery
  :: [ Either StitchVomitError ( Schema VALID ) ]
  -> [ Either StitchVomitError ( Schema VALID ) ]
filterQuery schemas = filterSchema schemas "Query"

-- | Get first matching query. Its purpose is to serve as some sort of accumulator
-- for the rest of the Queries. In other words, get the first query and stick the
-- rest of the matching queries into this one.
getFirstQuery
  :: [ Either StitchVomitError ( Schema VALID ) ]
  -> Either StitchVomitError ( Schema VALID )
getFirstQuery parsedDocs = head $ fromList $ take 1 $ parsedDocs

-- | Render schema as human readable text.
renderSchema :: ( Schema VALID ) -> ByteString
renderSchema = BL.toStrict . render

updateSchemaEntries
  :: HashMap FieldName ( Indexed FieldName ( FieldDefinition OUT VALID ) )
  -> Schema VALID
  -> Schema VALID
updateSchemaEntries newMapEntries s = s
  & queryL
  . typeContentL
  . objectFieldsL
  . mapEntriesL
  .~ newMapEntries

-- | Empty the types field.
mkEmptyTypes :: Schema s -> Schema s
mkEmptyTypes s = s & typesL .~ ( unsafeFromList [] )

updateSchemaTypes
  :: [ Either StitchVomitError ( Schema VALID ) ]
  -> Either StitchVomitError ( Schema VALID )
  -> Either StitchVomitError ( Schema VALID )
updateSchemaTypes parsedDocs updatedSchema = case updatedSchema of
  Left err -> Left err
  Right schema ->
    let
      ts :: [ TypeLib VALID ]
      ts = foldr
        (\a b -> case a of
            Left _  -> b
            Right d -> d : b
            )
        [] ( parsedDocs <&> (\parsedDoc -> parsedDoc <&> ( ^. typesL ) ) )
    in Right $ schema & typesL .~ ( unsafeFromList $ HS.toList $ HS.unions $ unsafeToHashMap <$> ts )

-- * Lens Util

typesL :: Lens' ( Schema a ) ( TypeLib a )
typesL = lens types (\s newTypeLib -> s { types = newTypeLib })

queryL :: Lens' ( Schema VALID ) ( TypeDefinition OBJECT VALID )
queryL = lens query (\q newQuery -> q { query = newQuery })

typeNameL :: Lens' ( TypeDefinition a s ) TypeName
typeNameL = lens typeName (\td newTypeName -> td { typeName = newTypeName })

typeContentL :: Lens' ( TypeDefinition a s ) ( TypeContent TRUE a s )
typeContentL = lens typeContent (\s newTypeContent -> s { typeContent = newTypeContent })

objectFieldsL :: Lens' ( TypeContent ( ELEM OBJECT a ) a s ) ( FieldsDefinition OUT s )
objectFieldsL = lens objectFields (\dt newObjectFields -> dt { objectFields = newObjectFields })

mapEntriesL :: Lens' ( OrdMap k a ) ( HashMap k ( Indexed k a ) )
mapEntriesL = lens mapEntries (\om newOrdMap -> om { mapEntries = newOrdMap })

