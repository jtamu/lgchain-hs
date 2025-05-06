{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O))
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Lgchain.Core.Clients (Chain (StrChain), invoke, strOutput, LgchainError(..))
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
main = void $ runMaybeT $ do
  -- 履歴初期化
  liftIO $ migrate sampleHistory
  liftIO $ deleteMessages sampleHistory
  liftIO $ mapM_ (addMessage sampleHistory) sampleHistories

  -- LLM呼び出し
  messages <- liftIO $ getMessages sampleHistory
  let userMessage = ReqMessage User "Do you understand my name?"
  let prompt =
        [ReqMessage System "You are a helpful assistant."]
          ++ messages
          ++ [userMessage]
  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt

  -- 履歴保存
  res <- MaybeT $ do
    result <- runExceptT $ invoke chain Nothing
    return $ case result of
      Left _ -> Nothing
      Right output -> strOutput output
  liftIO $ addMessage sampleHistory userMessage
  liftIO $ addMessage sampleHistory . ReqMessage Assistant . T.pack $ res
  liftIO $ getMessages sampleHistory >>= print
