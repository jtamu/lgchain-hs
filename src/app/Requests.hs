{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Data.Aeson (ToJSON, Value (String), object, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (toJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

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

-- JSON Schema for structured output
data JsonSchemaProperty = JsonSchemaProperty
  { description :: Maybe Text,
    propertyType :: Text,
    items :: Maybe JsonSchemaProperty,
    uniqueItems :: Maybe Bool
  }
  deriving (Show, Generic)

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
  deriving (Show, Generic)

instance ToJSON JsonSchema where
  toJSON (JsonSchema schemaType properties required) =
    object ["type" .= schemaType, "properties" .= object [fromText k .= v | (k, v) <- properties], "required" .= required]

data JsonSchemaDefinition = JsonSchemaDefinition
  { name :: Text,
    schema :: JsonSchema
  }
  deriving (Show, Generic)

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

sampleResFormat :: ResponseFormat
sampleResFormat =
  ResponseFormat
    { formatType = "json_schema",
      jsonSchema =
        JsonSchemaDefinition
          { name = "Recipe",
            schema =
              JsonSchema
                { schemaType = "object",
                  properties =
                    [ ( "ingredients",
                        JsonSchemaProperty
                          { description = Just "ingredients of the dish",
                            propertyType = "array",
                            items =
                              Just
                                ( JsonSchemaProperty
                                    { description = Nothing,
                                      propertyType = "string",
                                      items = Nothing,
                                      uniqueItems = Nothing
                                    }
                                ),
                            uniqueItems = Just True
                          }
                      ),
                      ( "steps",
                        JsonSchemaProperty
                          { description = Just "steps to make the dish",
                            propertyType = "array",
                            items =
                              Just
                                ( JsonSchemaProperty
                                    { description = Nothing,
                                      propertyType = "string",
                                      items = Nothing,
                                      uniqueItems = Nothing
                                    }
                                ),
                            uniqueItems = Just True
                          }
                      )
                    ],
                  required = ["ingredients", "steps"]
                }
          }
    }
