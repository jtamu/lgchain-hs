{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.MCP.Clients.Requests where

import Data.Aeson (ToJSON (toJSON), Value, object, (.=))
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Lgchain.Core.Requests (ViewableText)

data Request = Request
  { requestJsonRpc :: ViewableText,
    requestMethod :: ViewableText,
    requestId :: ViewableText,
    requestParams :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON Request where
  toJSON (Request jsonrpc method requestId params) =
    object $ ["jsonrpc" .= jsonrpc, "method" .= method, "id" .= requestId] ++ maybe [] (\p -> ["params" .= p]) params

data Notification = Notification
  { notificationJsonRpc :: ViewableText,
    notificationMethod :: ViewableText
  }
  deriving (Show, Generic)

instance ToJSON Notification where
  toJSON (Notification jsonrpc method) =
    object ["jsonrpc" .= jsonrpc, "method" .= method]

-- MCPのtools/callリクエスト用の型定義

type ToolCallParams = M.Map Text Value
