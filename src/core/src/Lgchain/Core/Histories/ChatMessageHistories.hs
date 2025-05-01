module Lgchain.Core.Histories.ChatMessageHistories where

import Lgchain.Core.Requests (ReqMessage)

class ChatMessageHistory a e where
  getMessages :: (Eq e) => a -> e -> [ReqMessage]
  addMessage :: (Eq e) => a -> e -> ReqMessage -> a

data ChatHistoryData a = ChatHistoryData
  { sessionId :: a,
    message :: ReqMessage
  }
  deriving (Show)

newtype InMemoryChatMessageHistory a = InMemoryChatMessageHistory [ChatHistoryData a] deriving (Show)

instance ChatMessageHistory (InMemoryChatMessageHistory a) a where
  getMessages (InMemoryChatMessageHistory histories) sessionId' =
    [message | ChatHistoryData sessionId message <- histories, sessionId == sessionId']

  addMessage (InMemoryChatMessageHistory histories) sessionId' message' =
    InMemoryChatMessageHistory (reverse $ ChatHistoryData sessionId' message' : reverse histories)
