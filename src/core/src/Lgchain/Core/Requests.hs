module Lgchain.Core.Requests where

import Data.Map qualified as M
import Data.Text qualified as T
import GHC.Generics (Generic)

data Role = System | User | Model deriving (Eq)

data ReqMessage = ReqMessage {role :: Role, content :: T.Text} deriving (Eq, Generic)

type Prompt = [ReqMessage]

type FormatMap = M.Map T.Text T.Text

formatAll :: T.Text -> FormatMap -> T.Text
formatAll = M.foldlWithKey (\acc k v -> T.replace k v acc)

formatPrompt :: FormatMap -> Prompt -> Prompt
formatPrompt formatMap prompt = [ReqMessage role (formatAll content formatMap) | ReqMessage role content <- prompt]
