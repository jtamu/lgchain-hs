{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.MCP.Clients.Responses where

import Data.Aeson (FromJSON (parseJSON), withObject, (.:), (.:?))
import Data.Map (Map)
import GHC.Base ((<|>))
import GHC.Generics (Generic)
import Lgchain.Core.Clients (LgchainError (ApiError))
import Lgchain.Core.Requests (vpack)

newtype Response a = Response {getResponse :: Either ErrorResponse (SuccessResponse a)}
  deriving (Show, Generic)

instance (FromJSON a) => FromJSON (Response a) where
  parseJSON v = Response <$> (Left <$> parseJSON v <|> Right <$> parseJSON v)

data SuccessResponse a = SuccessResponse
  { successResponseJsonRpc :: String,
    successResponseResult :: a,
    successResponseId :: String
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
    jsonRpcErrorMessage :: String
  }
  deriving (Show, Generic)

instance FromJSON JsonRpcError where
  parseJSON = withObject "JsonRpcError" $ \o ->
    JsonRpcError
      <$> o .: "code"
      <*> o .: "message"

data ErrorResponse = ErrorResponse
  { errorResponseJsonRpc :: String,
    errorResponseError :: JsonRpcError,
    errorResponseId :: String
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
  { propertyType :: String,
    propertyDescription :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON SchemaProperty where
  parseJSON = withObject "SchemaProperty" $ \o ->
    SchemaProperty
      <$> o .: "type"
      <*> o .:? "description"

data InputSchema = InputSchema
  { schemaType :: String,
    schemaProperties :: Map String SchemaProperty,
    schemaRequired :: [String]
  }
  deriving (Show, Generic)

instance FromJSON InputSchema where
  parseJSON = withObject "InputSchema" $ \o ->
    InputSchema
      <$> o .: "type"
      <*> o .: "properties"
      <*> o .: "required"

data Tool = Tool
  { toolName :: String,
    toolDescription :: String,
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
    nextCursor :: Maybe String
  }
  deriving (Show, Generic)

instance FromJSON ToolsListResult where
  parseJSON = withObject "ToolsListResult" $ \o ->
    ToolsListResult
      <$> o .: "tools"
      <*> o .:? "nextCursor"

extractToolsFromSuccessResponse :: SuccessResponse ToolsListResult -> [Tool]
extractToolsFromSuccessResponse (SuccessResponse _ toolsListResult _) = tools toolsListResult

errorResponseToLgchainError :: ErrorResponse -> LgchainError
errorResponseToLgchainError (ErrorResponse _ err _) =
  ApiError $
    vpack $
      "code: " ++ show (jsonRpcErrorCode err) ++ " message: " ++ jsonRpcErrorMessage err
