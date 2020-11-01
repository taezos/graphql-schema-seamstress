{-# LANGUAGE GADTs               #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.GqlSchema.Stitch where

-- bytestring
import qualified Data.ByteString.Char8            as BS

-- directory
import           System.Directory

-- filepath
import           System.FilePath.Posix

-- graphql-stitch-vomit
import           Import

-- morpheus-graphql-core
import           Data.Morpheus.Core
import           Data.Morpheus.Ext.Map
import           Data.Morpheus.Ext.SafeHashMap
import           Data.Morpheus.Types.Internal.AST

-- bytestring
import qualified Data.ByteString.Lazy.Char8       as BL

-- microlens
import           Lens.Micro

-- unordered-containers
import qualified Data.HashMap.Strict              as HS

import Control.Exception

-- | Stitches all the queries in one file.
stitchQuery
  :: FilePath
  -- ^ Source directory of schema.
  -> IO ( Either String ( Schema VALID ) )
stitchQuery fp = do
  filePaths <- listDirectory fp
  let
    validFilePaths = makeValid
      . (\fps -> fp <> ( pathSeparator : fps ) )
      <$> filePaths
  docs <- sequence $ BS.readFile <$> validFilePaths
  pure $ updateSchema $ parseDocument <$> docs

-- | Vomits all the queries in one file
vomitQuery
  :: FilePath
  -- ^ Target file.
  -> Either String ( Schema VALID ) -> IO ( Either String () )
vomitQuery fp schema = case schema of
  Left err -> pure $ Left err
  Right s -> tryJust (\(e :: IOException ) -> Just $ show e )
    $ writeSchema fp ( renderSchema s )

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
filterQuery parsedDocs =
  -- TODO: replace with foldr
  (\parseDoc -> case parseDoc of
      Left err -> Left err : []
      Right schema ->
        if typeNameQuery == ( schema ^. queryL . typeNameL & readTypeName )
        then Right ( mkEmptyTypes schema ) : []
        else []
  ) =<< parsedDocs

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

-- TODO: get all mapEntries from Schema Valid
updateSchema :: [ Either String ( Schema VALID ) ] -> Either String ( Schema VALID )
updateSchema parsedDocs = case getFirstQuery parsedDocs of
  Left err -> Left err
  Right schema ->
    let
      -- mapEntries
      me :: [ HashMap FieldName ( Indexed FieldName ( FieldDefinition OUT VALID ) ) ]
      me = foldr (\a _ -> a ^. queryL . typeContentL . objectFieldsL . mapEntriesL ) HS.empty <$> parsedDocs

    in Right $ updateSchemaEntries ( HS.unions me ) schema

updateSchemaEntries :: HashMap FieldName ( Indexed FieldName ( FieldDefinition OUT VALID ) ) -> Schema VALID -> Schema VALID
updateSchemaEntries newMapEntries s = s
  & queryL
  . typeContentL
  . objectFieldsL
  . mapEntriesL
  .~ newMapEntries

-- | Empty the types field.
mkEmptyTypes :: Schema s -> Schema s
mkEmptyTypes s = s & typesL .~ ( unsafeFromList [] )

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
