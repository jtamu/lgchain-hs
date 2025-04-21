module Responses where

import Data.Aeson (FromJSON)
import GHC.Generics (Generic)

data ResMessageContent = ResMessageContent {role :: String, content :: String} deriving (Show, Generic)

instance FromJSON ResMessageContent

newtype ResMessage = ResMessage {message :: ResMessageContent} deriving (Show, Generic)

instance FromJSON ResMessage

newtype ResBody = ResBody {choices :: [ResMessage]} deriving (Show, Generic)

instance FromJSON ResBody
