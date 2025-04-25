{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Requests where

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
import Data.Text qualified as T
import GHC.Generics (Generic)
import Language.Haskell.TH (Con (RecC), Dec (DataD), Info (TyConI), Name, Q, Type (AppT, ConT, ListT), conT, reify)
import Language.Haskell.TH.Syntax (Lift)
import Utils (takeAfterLastDot)

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

-- | リクエストのメインデータ型
data GenerateContentRequest = GenerateContentRequest
  { contents :: [Content],
    generationConfig :: Maybe GenerationConfig
  }
  deriving (Show, Eq, Generic)

instance FromJSON GenerateContentRequest

instance ToJSON GenerateContentRequest

class (FromJSON a, Show a, Eq a) => JsonSchemaConvertable a where
  convertJson :: a -> GenerationConfig

deriveJSONSchema :: Name -> Q [Dec]
deriveJSONSchema name = do
  TyConI (DataD _ _ _ _ [RecC _ fields] _) <- reify name
  let tup = [(fname, ftype) | (fname, _, ftype) <- fields]
      edefinition = mapGenerationConfig name tup
   in case edefinition of
        Right definition -> [d|instance JsonSchemaConvertable $(conT name) where convertJson _ = definition|]
        Left e -> fail e

mapGenerationConfig :: Name -> [(Name, Type)] -> Either String GenerationConfig
mapGenerationConfig _ props = do
  schemaProps <- sequenceA [mapSchema tup | tup <- props]
  return $
    GenerationConfig
      "application/json"
      ( SchemaItems ObjectType (M.fromList schemaProps) Nothing [T.pack $ takeAfterLastDot $ show name | (name, _) <- props]
      )

mapSchema :: (Name, Type) -> Either String (Text, SchemaItems)
mapSchema (name, typ) = do
  prop <- foldSchemaProperty typ
  return (T.pack $ takeAfterLastDot $ show name, prop)

foldSchemaProperty :: Type -> Either String SchemaItems
foldSchemaProperty (ConT a)
  | a == ''String =
      Right $ SchemaItems StringType M.empty Nothing []
  | a == ''Int =
      Right $ SchemaItems IntType M.empty Nothing []
  | a == ''Double =
      Right $ SchemaItems DoubleType M.empty Nothing []
  | a == ''Bool =
      Right $ SchemaItems BooleanType M.empty Nothing []
foldSchemaProperty (AppT ListT (ConT a)) = case foldSchemaProperty (ConT a) of
  Right prop ->
    Right $ SchemaItems ListType M.empty (Just prop) []
  Left err -> Left err
foldSchemaProperty _ =
  Left "Only String, Int, Double, Bool, [String], [Int], [Double], [Bool] are supported"
