{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import GHC.Generics (Generic)
import Lgchain.Core.Clients (Chain (StrChain), invoke, runOrFail, strOutput)
import Lgchain.Core.Histories.ChatMessageHistories (ChatMessageHistory (addMessage, deleteMessages), getMessages)
import Lgchain.Core.Histories.ChatMessageHistories.RDB (SqliteChatMessageHistory (SqliteChatMessageHistory), migrate)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (Assistant, System, User), ViewableText, deriveJsonSchema)
import Lgchain.OpenAI.Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O))

data Recipe = Recipe
  { ingredients :: [ViewableText],
    steps :: [ViewableText]
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Recipe

sampleHistories :: [ReqMessage]
sampleHistories =
  [ ReqMessage User "私の名前はトムです",
    ReqMessage Assistant "こんにちは、トムさん！"
  ]

sampleHistory :: SqliteChatMessageHistory
sampleHistory = SqliteChatMessageHistory "database.db" "2"

main :: IO ()
main = runOrFail $ do
  -- 履歴初期化
  liftIO $ migrate sampleHistory
  liftIO $ deleteMessages sampleHistory
  liftIO $ mapM_ (addMessage sampleHistory) sampleHistories

  -- LLM呼び出し
  messages <- liftIO $ getMessages sampleHistory
  let userMessage = ReqMessage User "私の名前を覚えていますか？"
  let prompt =
        [ReqMessage System "You are a helpful assistant."]
          ++ messages
          ++ [userMessage]
  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt

  -- 履歴保存
  result <- invoke chain Nothing
  res <- ExceptT $ return $ strOutput result
  liftIO $ addMessage sampleHistory userMessage
  liftIO $ addMessage sampleHistory . ReqMessage Assistant $ res
  finalMessages <- liftIO $ getMessages sampleHistory
  liftIO $ print finalMessages
