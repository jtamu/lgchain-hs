{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lgchain.Gemini.Requests where

import Data.Aeson
  ( FromJSON (parseJSON),
    ToJSON (toJSON),
    Value (String),
    object,
    withObject,
    (.:),
    (.:?),
    (.=),
  )
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift)
import Lgchain.Core.Requests qualified as Core (Function (parameters), FunctionParameter (FunctionParameter), JsonSchema (properties), JsonSchemaDefinition (JsonSchemaDefinition), JsonSchemaProperty (JsonSchemaProperty), PropertyType (BooleanType, DoubleType, IntType, ListType, StringType), Role (Assistant, System, User), Tool (Tool), functionDescription, functionName)

newtype Role = Role Core.Role

instance Eq Role where
  -- System と User はgeminiにおいては等価なものとして扱う (Systemは存在しないため)
  (==) (Role Core.System) (Role Core.User) = True
  (==) (Role Core.User) (Role Core.System) = True
  (==) (Role a) (Role b) = a == b

instance Show Role where
  show (Role Core.System) = "user"
  show (Role Core.User) = "user"
  show (Role Core.Assistant) = "assistant"

instance FromJSON Role where
  parseJSON (String s)
    | s == "user" = pure $ Role Core.User
    | s == "assistant" = pure $ Role Core.Assistant
  parseJSON _ = fail "Expected \"assistant\" or \"user\""

instance ToJSON Role where
  toJSON (Role Core.System) = String "user"
  toJSON (Role Core.User) = String "user"
  toJSON (Role Core.Assistant) = String "assistant"

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

-- | スキーマアイテム型
data PropertyType = IntType | StringType | DoubleType | BooleanType | ListType | ObjectType deriving (Eq, Show, Lift)

instance ToJSON PropertyType where
  toJSON IntType = String "integer"
  toJSON StringType = String "string"
  toJSON DoubleType = String "number"
  toJSON BooleanType = String "boolean"
  toJSON ListType = String "array"
  toJSON ObjectType = String "object"

instance FromJSON PropertyType where
  parseJSON (String s)
    | s == "integer" = pure IntType
    | s == "string" = pure StringType
    | s == "number" = pure DoubleType
    | s == "boolean" = pure BooleanType
    | s == "array" = pure ListType
    | s == "object" = pure ObjectType
  parseJSON _ = fail "Expected \"integer\", \"string\", \"number\", \"boolean\" or \"array\" or \"object\""

data SchemaItems = SchemaItems
  { itemsType :: PropertyType,
    properties :: M.Map Text SchemaItems,
    items :: Maybe SchemaItems,
    required :: [Text]
  }
  deriving (Show, Eq, Generic, Lift)

instance FromJSON SchemaItems where
  parseJSON = withObject "SchemaItems" $ \v ->
    SchemaItems <$> v .: "type" <*> v .: "properties" <*> v .:? "items" <*> v .: "required"

instance ToJSON SchemaItems where
  toJSON (SchemaItems itemsType properties items required) =
    object $ ["type" .= itemsType, "properties" .= properties, "required" .= required] ++ maybe [] (\x -> ["items" .= x]) items

-- | 生成設定型
data GenerationConfig = GenerationConfig
  { responseMimeType :: Text,
    responseSchema :: SchemaItems
  }
  deriving (Show, Eq, Generic, Lift)

instance FromJSON GenerationConfig where
  parseJSON = withObject "GenerationConfig" $ \v ->
    GenerationConfig <$> v .: "response_mime_type" <*> v .: "response_schema"

instance ToJSON GenerationConfig where
  toJSON :: GenerationConfig -> Value
  toJSON (GenerationConfig responseMimeType responseSchema) =
    object ["response_mime_type" .= responseMimeType, "response_schema" .= responseSchema]

-- | パラメータ型
data Parameter = Parameter
  { parameterType :: PropertyType,
    parameterDescription :: Maybe Text,
    parameterEnum :: Maybe [Text],
    parameterProperties :: Maybe (M.Map Text Parameter),
    parameterRequired :: Maybe [Text]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Parameter where
  parseJSON = withObject "Parameter" $ \v ->
    Parameter
      <$> v .: "type"
      <*> v .:? "description"
      <*> v .:? "enum"
      <*> v .:? "properties"
      <*> v .:? "required"

instance ToJSON Parameter where
  toJSON (Parameter paramType desc enum props required) =
    object $
      ["type" .= paramType]
        ++ maybe [] (\x -> ["description" .= x]) desc
        ++ maybe [] (\x -> ["enum" .= x]) enum
        ++ maybe [] (\x -> ["properties" .= x]) props
        ++ maybe [] (\x -> ["required" .= x]) required

-- | 関数宣言型
data FunctionDeclaration = FunctionDeclaration
  { name :: Text,
    description :: Text,
    parameters :: Parameter
  }
  deriving (Show, Eq, Generic)

instance FromJSON FunctionDeclaration

instance ToJSON FunctionDeclaration

-- | ツール型
newtype Tool = Tool
  { functionDeclarations :: [FunctionDeclaration]
  }
  deriving (Show, Eq, Generic)

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \v ->
    Tool <$> v .: "function_declarations"

instance ToJSON Tool where
  toJSON (Tool functionDeclarations) =
    object ["function_declarations" .= functionDeclarations]

-- | リクエストのメインデータ型
data GenerateContentRequest = GenerateContentRequest
  { contents :: [Content],
    tools :: Maybe [Tool],
    generationConfig :: Maybe GenerationConfig
  }
  deriving (Show, Eq, Generic)

instance FromJSON GenerateContentRequest

instance ToJSON GenerateContentRequest

-- | 共通のスキーマ定義をgemini用に変換する
mapCommonSchemaDefinition :: Core.JsonSchemaDefinition -> GenerationConfig
mapCommonSchemaDefinition (Core.JsonSchemaDefinition _ schema) =
  let itemsMap = M.map mapCommonSchemaProperty (Core.properties schema)
   in GenerationConfig "application/json" (SchemaItems ObjectType itemsMap Nothing (M.keys itemsMap))

mapCommonSchemaProperty :: Core.JsonSchemaProperty -> SchemaItems
mapCommonSchemaProperty (Core.JsonSchemaProperty _ propertyType items _) =
  SchemaItems typ M.empty (mapCommonSchemaProperty <$> items) []
  where
    typ = case propertyType of
      Core.IntType -> IntType
      Core.StringType -> StringType
      Core.DoubleType -> DoubleType
      Core.BooleanType -> BooleanType
      Core.ListType -> ListType

mapCommonTool :: Core.Tool -> Tool
mapCommonTool tool = Tool [mapCommonToolToFunc tool]

mapCommonToolToFunc :: Core.Tool -> FunctionDeclaration
mapCommonToolToFunc (Core.Tool _ func) = FunctionDeclaration name description parameters
  where
    name = Core.functionName func
    description = Core.functionDescription func
    parameters = mapCommonFunctionParameter (Core.parameters func)

mapCommonFunctionParameter :: Core.FunctionParameter -> Parameter
mapCommonFunctionParameter (Core.FunctionParameter _ paramProperties paramRequired _) =
  Parameter
    { parameterType = ObjectType,
      parameterDescription = Nothing,
      parameterEnum = Nothing,
      parameterProperties = Just $ M.map convertJsonSchemaProperty paramProperties,
      parameterRequired = Just paramRequired
    }

-- | JsonSchemaPropertyをParameterに変換する
convertJsonSchemaProperty :: Core.JsonSchemaProperty -> Parameter
convertJsonSchemaProperty (Core.JsonSchemaProperty desc propType _ _) =
  Parameter
    { parameterType = mapPropertyType propType,
      parameterDescription = desc,
      parameterEnum = Nothing,
      parameterProperties = Nothing,
      parameterRequired = Nothing
    }

-- | Core.PropertyTypeをPropertyTypeに変換
mapPropertyType :: Core.PropertyType -> PropertyType
mapPropertyType Core.IntType = IntType
mapPropertyType Core.StringType = StringType
mapPropertyType Core.DoubleType = DoubleType
mapPropertyType Core.BooleanType = BooleanType
mapPropertyType Core.ListType = ListType
