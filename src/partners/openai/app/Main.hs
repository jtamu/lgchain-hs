{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O))
import Data.Functor ((<&>))
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lgchain.Core.Clients (Chain (StrChain), invoke, strOutput)
import Lgchain.Core.Histories.ChatMessageHistories (ChatMessageHistory (addMessage, deleteMessages), getMessages)
import Lgchain.Core.Histories.ChatMessageHistories.RDB (SqliteChatMessageHistory (SqliteChatMessageHistory), migrate)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (Assistant, System, User), deriveJsonSchema)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Recipe

sampleHistories :: [ReqMessage]
sampleHistories =
  [ ReqMessage User "My name is Tom.",
    ReqMessage Assistant "Hello Tom."
  ]

sampleHistory :: SqliteChatMessageHistory
sampleHistory = SqliteChatMessageHistory "database.db" "2"

main :: IO ()
main = do
  -- 履歴初期化
  migrate sampleHistory
  deleteMessages sampleHistory
  mapM_ (addMessage sampleHistory) sampleHistories

  -- LLM呼び出し
  messages <- getMessages sampleHistory
  let userMessage = ReqMessage User "Do you understand my name?"
  let prompt =
        [ReqMessage System "You are a helpful assistant."]
          ++ messages
          ++ [userMessage]
  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt

  -- 履歴保存
  maybeRes <- invoke chain Nothing <&> strOutput
  case maybeRes of
    Just res -> do
      addMessage sampleHistory userMessage
      addMessage sampleHistory . ReqMessage Assistant . T.pack $ res
      getMessages sampleHistory >>= print
    Nothing -> putStrLn "failed"
