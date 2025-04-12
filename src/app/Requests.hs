module Requests where

import Data.Aeson (ToJSON, Value (String))
import Data.Aeson.Types (toJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)

data Role = System | User

instance Show Role where
  show System = "system"
  show User = "user"

instance ToJSON Role where
  toJSON System = String (pack "system")
  toJSON User = String (pack "user")

data ReqMessage = ReqMessage {role :: Role, content :: Text} deriving (Show, Generic)

instance ToJSON ReqMessage

type Prompt = [ReqMessage]

data ReqBody = ReqBody {model :: String, messages :: Prompt} deriving (Show, Generic)

instance ToJSON ReqBody
