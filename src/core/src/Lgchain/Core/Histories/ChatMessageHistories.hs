module Lgchain.Core.Histories.ChatMessageHistories where

import Lgchain.Core.Requests (ReqMessage)

type SessionId = String

class ChatMessageHistory a where
  getMessages :: a -> IO [ReqMessage]
  addMessage :: a -> ReqMessage -> IO ()
  deleteMessages :: a -> IO ()
