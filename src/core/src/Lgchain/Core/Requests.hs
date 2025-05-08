{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Lgchain.Core.Requests where

import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (String), object, withObject, (.:), (.:?), (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (toJSON)
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.String (IsString (fromString))
import Data.Text (Text, pack)
import Data.Text qualified as T
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
import Lgchain.Core.Utils (takeAfterLastDot)

newtype ViewableText = ViewableText Text deriving (Eq, Ord, Generic, FromJSON, ToJSON)

viewable :: Text -> ViewableText
viewable = ViewableText

unviewable :: ViewableText -> Text
unviewable (ViewableText s) = s

vpack :: String -> ViewableText
vpack = ViewableText . pack

vunpack :: ViewableText -> String
vunpack (ViewableText s) = T.unpack s

instance IsString ViewableText where
  fromString = vpack

-- StringのデフォルトのShowインスタンス実装をバイパスすることで、Unicodeエスケープを防ぐ
instance Show ViewableText where
  show (ViewableText s) = T.unpack $ T.append "\"" $ T.append s "\""

data Role = System | User | Assistant deriving (Eq)

instance Read Role where
  readsPrec _ s
    | "system" `isPrefixOf` s = [(System, drop 6 s)]
    | "user" `isPrefixOf` s = [(User, drop 4 s)]
    | "assistant" `isPrefixOf` s = [(Assistant, drop 9 s)]
    | otherwise = []

instance Show Role where
  show System = "system"
  show User = "user"
  show Assistant = "assistant"

instance ToJSON Role where
  toJSON System = String $ T.pack "system"
  toJSON User = String $ T.pack "user"
  toJSON Assistant = String $ T.pack "assistant"

instance FromJSON Role where
  parseJSON (String s)
    | s == T.pack "system" = pure System
    | s == T.pack "user" = pure User
    | s == T.pack "assistant" = pure Assistant
  parseJSON _ = fail "invalid role"

data ReqMessage = ReqMessage {role :: Role, content :: ViewableText} deriving (Eq, Show, Generic)

instance ToJSON ReqMessage

instance FromJSON ReqMessage

type Prompt = [ReqMessage]

type FormatMap = M.Map ViewableText ViewableText

formatAll :: ViewableText -> FormatMap -> ViewableText
formatAll = M.foldlWithKey (\acc k v -> viewable $ T.replace (unviewable k) (unviewable v) (unviewable acc))

formatPrompt :: FormatMap -> Prompt -> Prompt
formatPrompt formatMap prompt = [ReqMessage role (formatAll content formatMap) | ReqMessage role content <- prompt]

data PropertyType = IntType | StringType | DoubleType | BooleanType | ListType deriving (Eq, Show, Lift)

instance ToJSON PropertyType where
  toJSON IntType = String $ pack "integer"
  toJSON StringType = String $ pack "string"
  toJSON DoubleType = String $ pack "number"
  toJSON BooleanType = String $ pack "boolean"
  toJSON ListType = String $ pack "array"

instance FromJSON PropertyType where
  parseJSON (String s)
    | s == pack "integer" = pure IntType
    | s == pack "string" = pure StringType
    | s == pack "number" = pure DoubleType
    | s == pack "boolean" = pure BooleanType
    | s == pack "array" = pure ListType
  parseJSON _ = fail "Expected one of: \"integer\", \"string\", \"number\", \"boolean\", \"array\""

-- JSON Schema for structured output
data JsonSchemaProperty = JsonSchemaProperty
  { description :: Maybe Text,
    propertyType :: PropertyType,
    items :: Maybe JsonSchemaProperty,
    uniqueItems :: Maybe Bool
  }
  deriving (Eq, Show, Generic, Lift)

instance ToJSON JsonSchemaProperty where
  toJSON (JsonSchemaProperty description propertyType items uniqueItems) =
    object $
      maybe [] (\x -> ["description" .= x]) description
        ++ ["type" .= propertyType]
        ++ maybe [] (\x -> ["items" .= x]) items
        ++ maybe [] (\x -> ["unique_items" .= x]) uniqueItems

data JsonSchema = JsonSchema
  { schemaType :: Text,
    properties :: M.Map Text JsonSchemaProperty,
    required :: [Text]
  }
  deriving (Eq, Show, Generic, Lift)

instance ToJSON JsonSchema where
  toJSON (JsonSchema schemaType properties required) =
    object ["type" .= schemaType, "properties" .= object [fromText k .= v | (k, v) <- M.toList properties], "required" .= required]

data JsonSchemaDefinition = JsonSchemaDefinition
  { name :: Text,
    schema :: JsonSchema
  }
  deriving (Eq, Show, Generic, Lift)

instance ToJSON JsonSchemaDefinition where
  toJSON (JsonSchemaDefinition name schema) =
    object ["name" .= name, "schema" .= schema]

data FormatType = TextFormat | JsonFormat deriving (Eq, Show)

instance ToJSON FormatType where
  toJSON TextFormat = String $ pack "text"
  toJSON JsonFormat = String $ pack "json_schema"

instance FromJSON FormatType where
  parseJSON (String s)
    | s == pack "text" = pure TextFormat
    | s == pack "json_schema" = pure JsonFormat
  parseJSON _ = fail "Expected \"text\" or \"json_schema\""

data ResponseFormat = ResponseFormat
  { formatType :: FormatType,
    jsonSchema :: JsonSchemaDefinition
  }
  deriving (Eq, Show, Generic)

instance ToJSON ResponseFormat where
  toJSON (ResponseFormat formatType jsonSchema) =
    object ["type" .= formatType, "json_schema" .= jsonSchema]

instance FromJSON ResponseFormat where
  parseJSON = withObject "ResponseFormat" $ \v ->
    ResponseFormat
      <$> v .: "type"
      <*> v .: "json_schema"

data ReqBody = ReqBody
  { model :: String,
    messages :: Prompt,
    responseFormat :: Maybe ResponseFormat
  }
  deriving (Eq, Show, Generic)

instance ToJSON ReqBody where
  toJSON (ReqBody model messages responseFormat) =
    object $
      ["model" .= model, "messages" .= messages]
        ++ maybe [] (\x -> ["response_format" .= x]) responseFormat

instance FromJSON JsonSchemaProperty where
  parseJSON = withObject "JsonSchemaProperty" $ \v ->
    JsonSchemaProperty
      <$> v .:? "description"
      <*> v .: "type"
      <*> v .:? "items"
      <*> v .:? "unique_items"

instance FromJSON JsonSchema where
  parseJSON = withObject "JsonSchema" $ \v -> do
    schemaType <- v .: "type"
    props <- v .: "properties"
    required <- v .: "required"
    return $ JsonSchema schemaType props required

instance FromJSON JsonSchemaDefinition where
  parseJSON = withObject "JsonSchemaDefinition" $ \v ->
    JsonSchemaDefinition
      <$> v .: "name"
      <*> v .: "schema"

instance FromJSON ReqBody where
  parseJSON = withObject "ReqBody" $ \v ->
    ReqBody
      <$> v .: "model"
      <*> v .: "messages"
      <*> v .:? "response_format"

class (FromJSON a, Show a, Eq a) => JsonSchemaConvertable a where
  convertJson :: a -> JsonSchemaDefinition

deriveJsonSchema :: Name -> Q [Dec]
deriveJsonSchema name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
  let tup = [(fname, ftype) | (fname, _, ftype) <- fields]
      edefinition = mapSchemaDefinition name tup
   in case edefinition of
        Right definition -> do
          jsInst <- [d|instance JsonSchemaConvertable $(conT name) where convertJson _ = definition|]
          jsonInst <- [d|instance FromJSON $(conT name)|]
          return $ jsInst ++ jsonInst
        Left e -> fail e

mapSchemaDefinition :: Name -> [(Name, Type)] -> Either String JsonSchemaDefinition
mapSchemaDefinition schemaName a = case sequenceA [mapSchema tup | tup <- a] of
  Right props ->
    Right
      JsonSchemaDefinition
        { name = pack $ takeAfterLastDot $ show schemaName,
          schema = JsonSchema {schemaType = "object", properties = M.fromList props, required = [name | (name, _) <- props]}
        }
  Left err -> Left err

mapSchema :: (Name, Type) -> Either String (Text, JsonSchemaProperty)
mapSchema (name, typ) = do
  prop <- foldSchemaProperty typ
  return (pack $ takeAfterLastDot $ show name, prop)

foldSchemaProperty :: Type -> Either String JsonSchemaProperty
foldSchemaProperty (ConT a)
  | a == ''ViewableText =
      Right $ JsonSchemaProperty {description = Nothing, propertyType = StringType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (ConT a)
  | a == ''String = Left "String is not supported. Use ViewableString instead."
foldSchemaProperty (ConT a)
  | a == ''Int =
      Right $ JsonSchemaProperty {description = Nothing, propertyType = IntType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (ConT a)
  | a == ''Double =
      Right $ JsonSchemaProperty {description = Nothing, propertyType = DoubleType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (ConT a)
  | a == ''Bool =
      Right $ JsonSchemaProperty {description = Nothing, propertyType = BooleanType, items = Nothing, uniqueItems = Nothing}
foldSchemaProperty (AppT ListT (ConT a)) = case foldSchemaProperty (ConT a) of
  Right prop ->
    Right $ JsonSchemaProperty {description = Nothing, propertyType = ListType, items = Just prop, uniqueItems = Just True}
  Left err -> Left err
foldSchemaProperty _ = Left "Unsupported type"
