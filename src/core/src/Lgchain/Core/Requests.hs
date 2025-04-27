module Lgchain.Core.Requests where

import Data.Text qualified as T
import GHC.Generics (Generic)

data Role = System | User | Model deriving (Eq)

data ReqMessage = ReqMessage {role :: Role, content :: T.Text} deriving (Eq, Generic)

type Prompt = [ReqMessage]
