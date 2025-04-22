{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Requests where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
    object,
    withObject,
    (.:),
    (.=),
  )
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)

data Role = User | Model deriving (Eq)

instance Show Role where
  show User = "user"
  show Model = "model"

instance ToJSON Role where
  toJSON User = String "user"
  toJSON Model = String "model"

instance FromJSON Role where
  parseJSON (String s)
    | s == "user" = pure User
    | s == "model" = pure Model
  parseJSON _ = fail "Expected \"model\" or \"user\""

-- | パート型
newtype Part = Part
  { text :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Part

instance ToJSON Part

-- | コンテンツ型
data Content = Content
  { role :: Role,
    parts :: [Part]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Content

instance ToJSON Content

-- | レスポンススキーマ型
data ResponseSchema = ResponseSchema
  { schemaType :: Text,
    items :: SchemaItems
  }
  deriving (Show, Eq, Generic)

instance FromJSON ResponseSchema where
  parseJSON = withObject "ResponseSchema" $ \v ->
    ResponseSchema <$> v .: "type" <*> v .: "items"

instance ToJSON ResponseSchema where
  toJSON (ResponseSchema schemaType items) =
    object ["type" .= schemaType, "items" .= items]

-- | スキーマアイテム型
data SchemaItems = SchemaItems
  { itemsType :: Text,
    properties :: M.Map Text ResponseSchema
  }
  deriving (Show, Eq, Generic)

instance FromJSON SchemaItems where
  parseJSON = withObject "SchemaItems" $ \v ->
    SchemaItems <$> v .: "type" <*> v .: "properties"

instance ToJSON SchemaItems where
  toJSON (SchemaItems itemsType properties) =
    object ["type" .= itemsType, "properties" .= properties]

-- | 生成設定型
data GenerationConfig = GenerationConfig
  { responseMimeType :: Text,
    responseSchema :: ResponseSchema
  }
  deriving (Show, Eq, Generic)

instance FromJSON GenerationConfig where
  parseJSON = withObject "GenerationConfig" $ \v ->
    GenerationConfig <$> v .: "response_mime_type" <*> v .: "response_schema"

instance ToJSON GenerationConfig where
  toJSON :: GenerationConfig -> Value
  toJSON (GenerationConfig responseMimeType responseSchema) =
    object ["response_mime_type" .= responseMimeType, "response_schema" .= responseSchema]

-- | リクエストのメインデータ型
data GenerateContentRequest = GenerateContentRequest
  { contents :: [Content],
    generationConfig :: Maybe GenerationConfig
  }
  deriving (Show, Eq, Generic)

instance FromJSON GenerateContentRequest

instance ToJSON GenerateContentRequest
