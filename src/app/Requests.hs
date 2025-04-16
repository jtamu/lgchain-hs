{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Requests where

import Data.Aeson (ToJSON, Value (String), object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (toJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Language.Haskell.TH
  ( Con (RecC),
    Dec (DataD),
    Info (TyConI),
    Name,
    Q,
    Type (AppT, ConT, ListT),
    conT,
    reify,
  )
import Language.Haskell.TH.Syntax (Lift)
import Utils (takeAfterLastDot)

data Role = System | User

instance Show Role where
  show System = "system"
  show User = "user"

instance ToJSON Role where
  toJSON System = String (pack "system")
  toJSON User = String (pack "user")

data ReqMessage = ReqMessage {role :: Role, content :: Text} deriving (Show, Generic)

instance ToJSON ReqMessage

type Prompt = [ReqMessage]

data PropertyType = IntType | StringType | DoubleType | BooleanType | ListType deriving (Show, Lift)

instance ToJSON PropertyType where
  toJSON IntType = String $ pack "integer"
  toJSON StringType = String $ pack "string"
  toJSON DoubleType = String $ pack "number"
  toJSON BooleanType = String $ pack "boolean"
  toJSON ListType = String $ pack "array"

-- JSON Schema for structured output
data JsonSchemaProperty = JsonSchemaProperty
  { description :: Maybe Text,
    propertyType :: PropertyType,
    items :: Maybe JsonSchemaProperty,
    uniqueItems :: Maybe Bool
  }
  deriving (Show, Generic, Lift)

instance ToJSON JsonSchemaProperty where
  toJSON (JsonSchemaProperty description propertyType items uniqueItems) =
    object $
      maybe [] (\x -> ["description" .= x]) description
        ++ ["type" .= propertyType]
        ++ maybe [] (\x -> ["items" .= x]) items
        ++ maybe [] (\x -> ["unique_items" .= x]) uniqueItems

data JsonSchema = JsonSchema
  { schemaType :: Text,
    properties :: [(Text, JsonSchemaProperty)],
    required :: [Text]
  }
  deriving (Show, Generic, Lift)

instance ToJSON JsonSchema where
  toJSON (JsonSchema schemaType properties required) =
    object ["type" .= schemaType, "properties" .= object [fromText k .= v | (k, v) <- properties], "required" .= required]

data JsonSchemaDefinition = JsonSchemaDefinition
  { name :: Text,
    schema :: JsonSchema
  }
  deriving (Show, Generic, Lift)

instance ToJSON JsonSchemaDefinition where
  toJSON (JsonSchemaDefinition name schema) =
    object ["name" .= name, "schema" .= schema]

data ResponseFormat = ResponseFormat
  { formatType :: Text,
    jsonSchema :: JsonSchemaDefinition
  }
  deriving (Show, Generic)

instance ToJSON ResponseFormat where
  toJSON (ResponseFormat formatType jsonSchema) =
    object ["type" .= formatType, "json_schema" .= jsonSchema]

data ReqBody = ReqBody
  { model :: String,
    messages :: Prompt,
    responseFormat :: Maybe ResponseFormat
  }
  deriving (Show, Generic)

instance ToJSON ReqBody where
  toJSON (ReqBody model messages responseFormat) =
    object $
      ["model" .= model, "messages" .= messages]
        ++ maybe [] (\x -> ["response_format" .= x]) responseFormat

class JsonSchemaConvertable a where
  convertJson :: a -> JsonSchemaDefinition

deriveJsonSchema :: Name -> Q [Dec]
deriveJsonSchema name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
  let tup = [(fname, ftype) | (fname, _, ftype) <- fields]
      edefinition = mapSchemaDefinition tup
   in case edefinition of
        Right definition -> [d|instance JsonSchemaConvertable $(conT name) where convertJson _ = definition|]
        Left e -> fail e

mapSchemaDefinition :: [(Name, Type)] -> Either String JsonSchemaDefinition
mapSchemaDefinition a = case sequenceA [mapSchema tup | tup <- a] of
  Right props ->
    -- TODO: name, required
    Right JsonSchemaDefinition {name = "Recipe", schema = JsonSchema {schemaType = "object", properties = props, required = [name | (name, _) <- props]}}
  Left err -> Left err

mapSchema :: (Name, Type) -> Either String (Text, JsonSchemaProperty)
mapSchema (name, typ) = do
  prop <- foldSchemaProperty typ
  return (pack $ takeAfterLastDot $ show name, prop)

foldSchemaProperty :: Type -> Either String JsonSchemaProperty
foldSchemaProperty (ConT a) | a == ''String = Right $ JsonSchemaProperty {description = Nothing, propertyType = StringType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (ConT a) | a == ''Int = Right $ JsonSchemaProperty {description = Nothing, propertyType = IntType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (ConT a) | a == ''Double = Right $ JsonSchemaProperty {description = Nothing, propertyType = DoubleType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (ConT a) | a == ''Bool = Right $ JsonSchemaProperty {description = Nothing, propertyType = BooleanType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (AppT ListT (ConT a)) = case foldSchemaProperty (ConT a) of
  Right prop -> Right $ JsonSchemaProperty {description = Nothing, propertyType = ListType, items = Just prop, uniqueItems = Just True}
  Left err -> Left err
foldSchemaProperty _ = Left "Unsupported type"
