{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.Histories.ChatMessageHistories.RDBSpec where

import Lgchain.Core.Histories.ChatMessageHistories (ChatMessageHistory(getMessages, addMessage, deleteMessages))
import Lgchain.Core.Histories.ChatMessageHistories.RDB (SqliteChatMessageHistory(SqliteChatMessageHistory), migrate)
import Lgchain.Core.Requests (ReqMessage(ReqMessage), Role(System, User, Assistant))
import Test.Hspec (Spec, describe, context, it, beforeAll, afterAll, shouldBe, shouldMatchList)
import Data.Text (Text)
import Control.Exception (bracket)
import System.IO.Temp (withSystemTempFile)

spec :: Spec
spec = describe "SqliteChatMessageHistory" $ do
  describe "getMessages" $ do
    it "returns empty list when no messages exist" $ do
      let history = SqliteChatMessageHistory ":memory:"
      _ <- migrate history
      messages <- getMessages history "test-session"
      messages `shouldBe` []

    it "returns messages for the specified session" $ do
      let history = SqliteChatMessageHistory ":memory:"
      _ <- migrate history
      
      let session1 = "session-1"
      let session2 = "session-2"
      let message1 = ReqMessage System "System message for session 1"
      let message2 = ReqMessage User "User message for session 1"
      let message3 = ReqMessage Assistant "Assistant message for session 1"
      let message4 = ReqMessage System "System message for session 2"
      
      _ <- addMessage history session1 message1
      _ <- addMessage history session1 message2
      _ <- addMessage history session1 message3
      _ <- addMessage history session2 message4
      
      messages1 <- getMessages history session1
      messages1 `shouldMatchList` [message1, message2, message3]
      
      messages2 <- getMessages history session2
      messages2 `shouldMatchList` [message4]

  describe "addMessage" $ do
    it "adds a message to the history" $ do
      let history = SqliteChatMessageHistory ":memory:"
      _ <- migrate history
      
      let sessionId = "test-session"
      let message = ReqMessage User "Test message"
      
      _ <- addMessage history sessionId message
      
      messages <- getMessages history sessionId
      messages `shouldBe` [message]
      
    it "adds multiple messages in order" $ do
      let history = SqliteChatMessageHistory ":memory:"
      _ <- migrate history
      
      let sessionId = "test-session"
      let message1 = ReqMessage System "System message"
      let message2 = ReqMessage User "User message"
      let message3 = ReqMessage Assistant "Assistant message"
      
      _ <- addMessage history sessionId message1
      _ <- addMessage history sessionId message2
      _ <- addMessage history sessionId message3
      
      messages <- getMessages history sessionId
      messages `shouldBe` [message1, message2, message3]

  describe "deleteMessages" $ do
    it "deletes all messages for a session" $ do
      let history = SqliteChatMessageHistory ":memory:"
      _ <- migrate history
      
      let session1 = "session-1"
      let session2 = "session-2"
      let message1 = ReqMessage System "System message for session 1"
      let message2 = ReqMessage User "User message for session 1"
      let message3 = ReqMessage System "System message for session 2"
      
      _ <- addMessage history session1 message1
      _ <- addMessage history session1 message2
      _ <- addMessage history session2 message3
      
      _ <- deleteMessages history session1
      
      messages1 <- getMessages history session1
      messages1 `shouldBe` []
      
      messages2 <- getMessages history session2
      messages2 `shouldBe` [message3]
      
    it "does nothing when deleting messages for a non-existent session" $ do
      let history = SqliteChatMessageHistory ":memory:"
      _ <- migrate history
      
      _ <- deleteMessages history "non-existent-session"
      
      messages <- getMessages history "non-existent-session"
      messages `shouldBe` []
