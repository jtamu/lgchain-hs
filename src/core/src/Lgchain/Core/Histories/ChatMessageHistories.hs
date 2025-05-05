module Lgchain.Core.Histories.ChatMessageHistories where

import Lgchain.Core.Requests (ReqMessage)

type SessionId = String

class ChatMessageHistory a where
  getMessages :: a -> SessionId -> IO [ReqMessage]
  addMessage :: a -> SessionId -> ReqMessage -> IO ()
  deleteMessages :: a -> SessionId -> IO ()

data ChatHistoryData = ChatHistoryData
  { sessionId :: SessionId,
    message :: ReqMessage
  }
