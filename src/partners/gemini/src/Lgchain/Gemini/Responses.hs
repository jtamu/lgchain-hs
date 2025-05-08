{-# LANGUAGE DeriveGeneric #-}

module Lgchain.Gemini.Responses where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | パート型
newtype Part = Part
  { text :: Text
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

extractResponseMessage :: GenerateContentResponse -> Text
extractResponseMessage (GenerateContentResponse candidates) =
  let messages = [text part | candidate <- candidates, part <- parts $ content candidate] in head messages
