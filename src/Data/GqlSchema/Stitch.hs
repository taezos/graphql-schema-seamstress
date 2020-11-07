{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.GqlSchema.Stitch where

-- base
import           Control.Exception                (IOException)

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
import           Control.Exception.Safe           (throwM, tryJust)

-- unordered-containers
import qualified Data.HashMap.Strict              as HS

import qualified Data.Text                        as T
import qualified Data.Text.Encoding               as TE

class Monad m => ManageQuery m where
  stitchQuery :: Schemas -> m ( Either StitchVomitError ( Schema VALID ) )
  readSchemas :: FilePath -> m Schemas
  vomitQuery :: FilePath -> Schema VALID -> m ( Either StitchVomitError FilePath )

type Schemas = [ ByteString ]

-- | Stitches all the type Queries in one file.
stitchQueryImpl
  :: Monad m
  => Schemas
  -> m ( Either StitchVomitError ( Schema VALID ) )
stitchQueryImpl schemas = pure
  . first ( StitchError . T.pack )
  . updateSchema
  $ parseDocument
  <$> schemas

-- | Reads all the schemas from the source directory.
readSchemasImpl
  :: MonadIO m
  => FilePath
  -- ^ Source directory of schema.
  -> m [ ByteString ]
readSchemasImpl fp = liftIO $ do
  -- TODO: try io exception.
  filePaths <- listDirectory fp
  let
    validFilePaths = makeValid
      . (\fps -> fp <> ( pathSeparator : fps ) )
      <$> filePaths
  sequence $ BS.readFile <$> validFilePaths

-- | Vomits all the queries in one file
vomitQueryImpl
  :: MonadIO m
  => FilePath
  -> Schema VALID
  -> m ( Either StitchVomitError FilePath )
vomitQueryImpl fp schema = do
  res <- liftIO $ tryJust (\( e :: IOException ) -> throwM $ VomitError $ show e)
    $ writeSchema fp ( vomitMessage <> renderSchema schema )
  absolutePath <- liftIO $ getCurrentDirectory
  pure $ second ( const $ absolutePath <> ( pathSeparator : fp ) ) res
  where
    vomitMessage :: ByteString
    vomitMessage = TE.encodeUtf8 $ unlines
      [ "\"\"\""
      , "DO NOT EDIT."
      , "THIS FILE IS MEANT TO BE OVERWRITTEN BY GRAPHQL-STITCH-VOMIT."
      , "YOU HAVE BEEN WARNED!!!"
      , "\"\"\""
      ]

parseDocument :: ByteString -> Either String ( Schema VALID )
parseDocument = parseDSL . BL.fromStrict

isTypeNameQuery :: ( Schema VALID ) -> Bool
isTypeNameQuery s = typeNameQuery == ( readTypeName . typeName . query $ s )

typeNameQuery :: Text
typeNameQuery = "Query"

-- | Filter for Query
filterQuery
  :: [ Either String ( Schema VALID ) ]
  -> [ Either String ( Schema VALID ) ]
filterQuery parsedDocs = foldr filterByTypeNameQuery [] parsedDocs
  where
    filterByTypeNameQuery
      :: Either String ( Schema VALID )
      -> [ Either String ( Schema VALID ) ]
      -> [ Either String ( Schema VALID ) ]
    filterByTypeNameQuery pd accum =  case pd of
      Left err -> Left err : accum
      Right schema ->
        if typeNameQuery == ( schema ^. queryL . typeNameL & readTypeName )
        then Right ( mkEmptyTypes schema ) : accum
        else accum

filterQuery'
  :: [ Either String ( Schema VALID ) ]
  -> [ Either String ( Schema VALID ) ]
filterQuery' parsedDocs = foldr filterByTypeNameQuery [] parsedDocs
  where
    filterByTypeNameQuery
      :: Either String ( Schema VALID )
      -> [ Either String ( Schema VALID ) ]
      -> [ Either String ( Schema VALID ) ]
    filterByTypeNameQuery pd accum =  case pd of
      Left err -> Left err : accum
      Right schema ->
        if typeNameQuery == ( schema ^. queryL . typeNameL & readTypeName )
        then Right ( mkEmptyTypes schema ) : accum
        else accum

getFirstQuery
  :: [ Either String ( Schema VALID ) ]
  -> Either String ( Schema VALID )
getFirstQuery parsedDocs = head $ fromList $ take 1 $ filterQuery parsedDocs

-- | Extract `TypeContent` from `Schema`
extractTypeContent
  :: [ Either String ( Schema VALID ) ]
  -> [ Either String ( TypeContent TRUE OBJECT VALID ) ]
extractTypeContent parsedQueries =
  -- TODO: replace with foldr
  (\parsedQuery -> case parsedQuery of
      Left err -> Left err : []
      Right q  -> Right ( q ^. queryL . typeContentL ) : []
  ) =<< parsedQueries

-- | Extract `objectFields` from `TypeContent`
extractObjectFields
  :: [ Either String ( Schema VALID ) ]
  -> [ Either String ( OrdMap FieldName ( FieldDefinition OUT VALID ) ) ]
extractObjectFields parsedQueries =
  -- TODO: replace with foldr
  (\parsedQuery -> case parsedQuery of
      Left err -> Left err : []
      Right q  -> Right ( q ^. queryL . typeContentL & extractObjectField ) : []
  ) =<< parsedQueries

extractObjectField
  :: TypeContent TRUE OBJECT VALID
  -> OrdMap FieldName ( FieldDefinition OUT VALID )
extractObjectField DataObject { objectFields } = objectFields

renderSchema :: ( Schema VALID ) -> ByteString
renderSchema = BL.toStrict . render

writeSchema :: FilePath -> ByteString -> IO ()
writeSchema fp bs = BS.writeFile fp bs

updateSchema
  :: [ Either String ( Schema VALID ) ]
  -> Either String ( Schema VALID )
updateSchema parsedDocs = updateSchemaEntries ( mkMapEntries parsedDocs )
  <$> getFirstQuery parsedDocs
  where
    mkMapEntries
      :: [ Either String ( Schema VALID ) ]
      -> HashMap FieldName ( Indexed FieldName ( FieldDefinition OUT VALID ) )
    mkMapEntries pds = HS.unions
      $ foldr (\a _ -> a ^. queryL . typeContentL . objectFieldsL . mapEntriesL ) HS.empty
      <$> pds

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
  :: [ Either String ( Schema VALID ) ]
  -> Either String ( Schema VALID )
  -> Either String ( Schema VALID )
updateSchemaTypes parsedDocs updatedSchema = case updatedSchema of
  Left err -> Left err
  Right schema ->
    let
      ts = parsedDocs <&> (\parsedDoc -> parsedDoc <&> ( ^. typesL ) )
      t = foldr
          (\a b -> case a of
              Left _  -> b
              Right d -> d : b
            ) [] ts
    in Right $ schema & typesL .~ ( unsafeFromList $ HS.toList $ HS.unions $ unsafeToHashMap <$> t )

-- TODO: remove, only for manual testing.
getTypes :: Schema a -> TypeLib a
getTypes s = s ^. typesL

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
