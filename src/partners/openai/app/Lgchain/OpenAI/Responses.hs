module Lgchain.OpenAI.Responses where

import Data.Aeson (FromJSON, parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Key (fromString)
import GHC.Generics (Generic)
import Lgchain.Core.Responses (ToolCall)

data ResMessageContent = ResMessageContent
  { role :: String,
    content :: Maybe String,
    toolCalls :: Maybe [ToolCall]
  }
  deriving (Show, Generic)

instance FromJSON ResMessageContent where
  parseJSON = withObject "ResMessageContent" $ \v ->
    ResMessageContent
      <$> v .: fromString "role"
      <*> v .:? fromString "content"
      <*> v .:? fromString "tool_calls"

newtype ResMessage = ResMessage {message :: ResMessageContent} deriving (Show, Generic)

instance FromJSON ResMessage

newtype ResBody = ResBody {choices :: [ResMessage]} deriving (Show, Generic)

instance FromJSON ResBody
