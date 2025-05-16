{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.MCP.Clients.Responses where

import Data.Aeson (FromJSON (parseJSON), withObject, (.!=), (.:), (.:?))
import Data.Map (Map)
import GHC.Base ((<|>))
import GHC.Generics (Generic)
import Lgchain.Core.Clients (LgchainError (ApiError))
import Lgchain.Core.Requests (ViewableText, append, vpack)

newtype Response a = Response {getResponse :: Either ErrorResponse (SuccessResponse a)}
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON v = Response <$> (Left <$> parseJSON v <|> Right <$> parseJSON v)

data SuccessResponse a = SuccessResponse
  { successResponseJsonRpc :: ViewableText,
    successResponseResult :: a,
    successResponseId :: ViewableText
  }
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (SuccessResponse a) where
  parseJSON = withObject "SuccessResponse" $ \o ->
    SuccessResponse
      <$> o .: "jsonrpc"
      <*> o .: "result"
      <*> o .: "id"

data JsonRpcError = JsonRpcError
  { jsonRpcErrorCode :: Int,
    jsonRpcErrorMessage :: ViewableText
  }
  deriving (Show, Generic)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o ->
    JsonRpcError
      <$> o .: "code"
      <*> o .: "message"

data ErrorResponse = ErrorResponse
  { errorResponseJsonRpc :: ViewableText,
    errorResponseError :: JsonRpcError,
    errorResponseId :: ViewableText
  }
  deriving (Show, Generic)

instance FromJSON ErrorResponse where
  parseJSON = withObject "ErrorResponse" $ \o ->
    ErrorResponse
      <$> o .: "jsonrpc"
      <*> o .: "error"
      <*> o .: "id"

-- MCPのtools/listレスポンス用の型定義

data SchemaProperty = SchemaProperty
  { propertyType :: ViewableText,
    propertyDescription :: Maybe ViewableText
  }
  deriving (Show, Generic)

instance FromJSON SchemaProperty where
  parseJSON = withObject "SchemaProperty" $ \o ->
    SchemaProperty
      <$> o .: "type"
      <*> o .:? "description"

data InputSchema = InputSchema
  { schemaType :: ViewableText,
    schemaProperties :: Map ViewableText SchemaProperty,
    schemaRequired :: [ViewableText]
  }
  deriving (Show, Generic)

instance FromJSON InputSchema where
  parseJSON = withObject "InputSchema" $ \o ->
    InputSchema
      <$> o .: "type"
      <*> o .: "properties"
      <*> o .: "required"

data Tool = Tool
  { toolName :: ViewableText,
    toolDescription :: ViewableText,
    toolInputSchema :: InputSchema
  }
  deriving (Show, Generic)

instance FromJSON Tool where
  parseJSON = withObject "Tool" $ \o ->
    Tool
      <$> o .: "name"
      <*> o .: "description"
      <*> o .: "inputSchema"

data ToolsListResult = ToolsListResult
  { tools :: [Tool],
    nextCursor :: Maybe ViewableText
  }
  deriving (Show, Generic)

instance FromJSON ToolsListResult where
  parseJSON = withObject "ToolsListResult" $ \o ->
    ToolsListResult
      <$> o .: "tools"
      <*> o .:? "nextCursor"

-- MCPのtools/callレスポンス用の型定義

-- リソース型
data Resource = Resource
  { resourceUri :: ViewableText,
    resourceMimeType :: ViewableText,
    resourceText :: Maybe ViewableText
  }
  deriving (Show, Generic)

instance FromJSON Resource where
  parseJSON = withObject "Resource" $ \o ->
    Resource
      <$> o .: "uri"
      <*> o .: "mimeType"
      <*> o .:? "text"

-- コンテンツ項目の型を分離
newtype TextContent = TextContent
  { textValue :: ViewableText
  }
  deriving (Show, Generic)

data BinaryContent = BinaryContent
  { binaryData :: ViewableText,
    binaryMimeType :: ViewableText
  }
  deriving (Show, Generic)

newtype ResourceContent = ResourceContent
  { resourceValue :: Resource
  }
  deriving (Show, Generic)

-- コンテンツ項目の代数的データ型
data ContentItem
  = TextItem TextContent
  | ImageItem BinaryContent
  | AudioItem BinaryContent
  | ResourceItem ResourceContent
  deriving (Show, Generic)

instance FromJSON ContentItem where
  parseJSON = withObject "ContentItem" $ \o -> do
    cType <- o .: "type"
    case cType of
      "text" -> TextItem . TextContent <$> (o .: "text")
      "image" -> ImageItem <$> (BinaryContent <$> o .: "data" <*> o .: "mimeType")
      "audio" -> AudioItem <$> (BinaryContent <$> o .: "data" <*> o .: "mimeType")
      "resource" -> ResourceItem . ResourceContent <$> (o .: "resource")
      _ -> fail $ "Unknown content type: " ++ cType

data ToolCallResult = ToolCallResult
  { content :: [ContentItem],
    isError :: Bool
  }
  deriving (Show, Generic)

instance FromJSON ToolCallResult where
  parseJSON = withObject "ToolCallResult" $ \o ->
    ToolCallResult
      <$> o .: "content"
      <*> o .:? "isError" .!= False

getContentItemsFromSuccessResponse :: SuccessResponse ToolCallResult -> [ContentItem]
getContentItemsFromSuccessResponse (SuccessResponse _ toolCallResult _) = content toolCallResult

getToolsFromSuccessResponse :: SuccessResponse ToolsListResult -> [Tool]
getToolsFromSuccessResponse (SuccessResponse _ toolsListResult _) = tools toolsListResult

errorResponseToLgchainError :: ErrorResponse -> LgchainError
errorResponseToLgchainError (ErrorResponse _ err _) =
  ApiError $
    "code: " `append` vpack (show (jsonRpcErrorCode err)) `append` " message: " `append` jsonRpcErrorMessage err
