{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.Histories.ChatMessageHistories.RDBSpec where

import Lgchain.Core.Histories.ChatMessageHistories (ChatMessageHistory(getMessages, addMessage, deleteMessages))
import Lgchain.Core.Histories.ChatMessageHistories.RDB (SqliteChatMessageHistory(SqliteChatMessageHistory), migrate)
import Lgchain.Core.Requests (ReqMessage(ReqMessage), Role(System, User, Assistant))
import Test.Hspec (Spec, describe, it, shouldBe, shouldMatchList)
import Data.Text (Text, pack)

spec :: Spec
spec = describe "SqliteChatMessageHistory" $ do
  describe "getMessages" $ do
    it "returns empty list when no messages exist" $ do
      let history = SqliteChatMessageHistory (Data.Text.pack ":memory:") "test-session"
      _ <- migrate history
      messages <- getMessages history
      messages `shouldBe` []

    it "returns messages for the specified session" $ do
      let dbPath = Data.Text.pack ":memory:"
      let history1 = SqliteChatMessageHistory dbPath "session-1"
      let history2 = SqliteChatMessageHistory dbPath "session-2"
      _ <- migrate history1
      _ <- migrate history2
      
      let message1 = ReqMessage System "System message for session 1"
      let message2 = ReqMessage User "User message for session 1"
      let message3 = ReqMessage Assistant "Assistant message for session 1"
      let message4 = ReqMessage System "System message for session 2"
      
      _ <- addMessage history1 message1
      _ <- addMessage history1 message2
      _ <- addMessage history1 message3
      _ <- addMessage history2 message4
      
      messages1 <- getMessages history1
      messages1 `shouldMatchList` [message1, message2, message3]
      
      messages2 <- getMessages history2
      messages2 `shouldMatchList` [message4]

  describe "addMessage" $ do
    it "adds a message to the history" $ do
      let sessionId = "test-session"
      let history = SqliteChatMessageHistory (Data.Text.pack ":memory:") sessionId
      _ <- migrate history
      
      let message = ReqMessage User "Test message"
      
      _ <- addMessage history message
      
      messages <- getMessages history
      messages `shouldBe` [message]
      
    it "adds multiple messages in order" $ do
      let sessionId = "test-session"
      let history = SqliteChatMessageHistory (Data.Text.pack ":memory:") sessionId
      _ <- migrate history
      
      let message1 = ReqMessage System "System message"
      let message2 = ReqMessage User "User message"
      let message3 = ReqMessage Assistant "Assistant message"
      
      _ <- addMessage history message1
      _ <- addMessage history message2
      _ <- addMessage history message3
      
      messages <- getMessages history
      messages `shouldBe` [message1, message2, message3]

  describe "deleteMessages" $ do
    it "deletes all messages for a session" $ do
      let dbPath = Data.Text.pack ":memory:"
      let history1 = SqliteChatMessageHistory dbPath "session-1"
      let history2 = SqliteChatMessageHistory dbPath "session-2"
      _ <- migrate history1
      _ <- migrate history2
      
      let message1 = ReqMessage System "System message for session 1"
      let message2 = ReqMessage User "User message for session 1"
      let message3 = ReqMessage System "System message for session 2"
      
      _ <- addMessage history1 message1
      _ <- addMessage history1 message2
      _ <- addMessage history2 message3
      
      _ <- deleteMessages history1
      
      messages1 <- getMessages history1
      messages1 `shouldBe` []
      
      messages2 <- getMessages history2
      messages2 `shouldBe` [message3]
      
    it "does nothing when deleting messages for a non-existent session" $ do
      let history = SqliteChatMessageHistory (Data.Text.pack ":memory:") "non-existent-session"
      _ <- migrate history
      
      _ <- deleteMessages history
      
      messages <- getMessages history
      messages `shouldBe` []
