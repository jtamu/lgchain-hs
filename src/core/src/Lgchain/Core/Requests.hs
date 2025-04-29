module Lgchain.Core.Requests where

import Data.Aeson (FromJSON (parseJSON), ToJSON (toJSON), Value (String))
import Data.Map qualified as M
import Data.Text qualified as T
import GHC.Generics (Generic)

data Role = System | User | Model deriving (Eq)

instance Show Role where
  show System = "system"
  show User = "user"
  show Model = "model"

instance ToJSON Role where
  toJSON System = String $ T.pack "system"
  toJSON User = String $ T.pack "user"
  toJSON Model = String $ T.pack "model"

instance FromJSON Role where
  parseJSON (String s)
    | s == T.pack "system" = pure System
    | s == T.pack "user" = pure User
    | s == T.pack "model" = pure Model
  parseJSON _ = fail "invalid role"

data ReqMessage = ReqMessage {role :: Role, content :: T.Text} deriving (Eq, Show, Generic)

instance ToJSON ReqMessage

instance FromJSON ReqMessage

type Prompt = [ReqMessage]

type FormatMap = M.Map T.Text T.Text

formatAll :: T.Text -> FormatMap -> T.Text
formatAll = M.foldlWithKey (\acc k v -> T.replace k v acc)

formatPrompt :: FormatMap -> Prompt -> Prompt
formatPrompt formatMap prompt = [ReqMessage role (formatAll content formatMap) | ReqMessage role content <- prompt]
