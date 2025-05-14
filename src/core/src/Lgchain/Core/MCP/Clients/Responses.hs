{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.MCP.Clients.Responses where

import Data.Aeson (FromJSON (parseJSON), Value, withObject, (.:))
import GHC.Base ((<|>))
import GHC.Generics (Generic)

newtype Response = Response {getResponse :: Either ErrorResponse SuccessResponse}
  deriving (Show, Generic)

instance FromJSON Response where
  parseJSON v = Response <$> (Left <$> parseJSON v <|> Right <$> parseJSON v)

data SuccessResponse = SuccessResponse
  { successResponseJsonRpc :: String,
    successResponseResult :: Value,
    successResponseId :: String
  }
  deriving (Show, Generic)

instance FromJSON SuccessResponse where
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
