{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O))
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lgchain.Core.Clients (Chain (StrChain), invoke, strOutput)
import Lgchain.Core.Histories.ChatMessageHistories (ChatHistoryData (ChatHistoryData), ChatMessageHistory (getMessages), InMemoryChatMessageHistory (InMemoryChatMessageHistory), addMessage)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (Assistant, User), deriveJsonSchema)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recipe

deriveJsonSchema ''Recipe

sampleHistory :: InMemoryChatMessageHistory Integer
sampleHistory =
  InMemoryChatMessageHistory
    [ ChatHistoryData 1 (ReqMessage User "Hello"),
      ChatHistoryData 1 (ReqMessage Assistant "Hi"),
      ChatHistoryData 1 (ReqMessage User "How are you?"),
      ChatHistoryData 2 (ReqMessage User "私の名前は田中です"),
      ChatHistoryData 2 (ReqMessage Assistant "田中さん、こんにちは")
    ]

main :: IO ()
main = do
  let prompt =
        getMessages sampleHistory (2 :: Integer)
          ++ [ ReqMessage User "私の名前がわかりますか？"
             ]
  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt

  res <- invoke chain Nothing <&> strOutput
  let updHistory = maybe sampleHistory (addMessage sampleHistory (2 :: Integer) . ReqMessage Assistant . T.pack) res
  print updHistory
