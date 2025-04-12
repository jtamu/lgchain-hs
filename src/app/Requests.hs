module Requests where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)

data ReqMessage = ReqMessage {role :: String, content :: String} deriving (Show, Generic)

instance ToJSON ReqMessage

type Prompt = [ReqMessage]

data ReqBody = ReqBody {model :: String, messages :: Prompt} deriving (Show, Generic)

instance ToJSON ReqBody
