{-# LANGUAGE DeriveGeneric #-}

module Lgchain.Gemini.Responses where

import Data.Aeson (FromJSON, ToJSON, Value (String))
import Data.Map qualified as M
import Data.Text (Text)
import GHC.Generics (Generic)
import Lgchain.Core.Responses qualified as Core

data FunctionCall = FunctionCall
  { name :: String,
    args :: M.Map Text Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON FunctionCall

instance ToJSON FunctionCall

-- | パート型
data Part = Part
  { text :: Maybe Text,
    functionCall :: Maybe FunctionCall
  }
  deriving (Show, Eq, Generic)

instance FromJSON Part

instance ToJSON Part

-- | コンテンツ型
data Content = Content
  { parts :: [Part],
    role :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON Content

instance ToJSON Content

-- | 候補型
newtype Candidate = Candidate
  { content :: Content
  }
  deriving (Show, Eq, Generic)

instance FromJSON Candidate

instance ToJSON Candidate

-- | レスポンスのメインデータ型
newtype GenerateContentResponse = GenerateContentResponse
  { candidates :: [Candidate]
  }
  deriving (Show, Eq, Generic)

instance FromJSON GenerateContentResponse

instance ToJSON GenerateContentResponse

extractResponseMessage :: GenerateContentResponse -> Maybe Text
extractResponseMessage (GenerateContentResponse candidates) =
  let messages = [text part | candidate <- candidates, part <- parts $ content candidate] in head messages

mapToolCall :: FunctionCall -> Core.ToolCall
mapToolCall (FunctionCall name args) = Core.ToolCall "function" "function" (Core.Function name (M.map String args))
