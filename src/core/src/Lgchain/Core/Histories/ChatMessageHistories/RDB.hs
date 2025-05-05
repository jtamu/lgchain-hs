{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lgchain.Core.Histories.ChatMessageHistories.RDB where

import Data.Maybe (catMaybes)
import Data.Text qualified as T
import Database.Persist (deleteWhere)
import Database.Persist.Sql
  ( BackendKey (SqlBackendKey),
    Entity (Entity),
    PersistEntity (Key),
    PersistStoreWrite (insert),
    runMigration,
    selectList,
    (==.),
  )
import Database.Persist.Sqlite
  ( runSqlite,
  )
import Database.Persist.TH
  ( mkMigrate,
    mkPersist,
    persistLowerCase,
    share,
    sqlSettings,
  )
import Lgchain.Core.Histories.ChatMessageHistories (ChatHistoryData (ChatHistoryData, message), ChatMessageHistory (deleteMessages), SessionId, addMessage, getMessages)
import Lgchain.Core.Requests (ReqMessage (ReqMessage, content, role))
import Text.Read (readMaybe)

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
ChatHistory
    sessionId String
    role String
    content String
    deriving Show
|]

migrate' :: T.Text -> IO ()
migrate' schemaName = runSqlite schemaName (runMigration migrateAll)

fromDomain :: ChatHistoryData -> ChatHistory
fromDomain (ChatHistoryData sessionId message) =
  ChatHistory (show sessionId) (show $ role message) (T.unpack $ content message)

toDomain :: Entity ChatHistory -> Maybe ChatHistoryData
toDomain (Entity _ history) = do
  readRole <- readMaybe (chatHistoryRole history)
  return $ ChatHistoryData (read $ chatHistorySessionId history) (ReqMessage readRole $ T.pack $ chatHistoryContent history)

saveHistory :: T.Text -> ChatHistory -> IO (Key ChatHistory)
saveHistory schemaName history = runSqlite schemaName $ insert history

getHistoriesBySessionId :: T.Text -> SessionId -> IO [Entity ChatHistory]
getHistoriesBySessionId schemaName sessionId = runSqlite schemaName $ selectList [ChatHistorySessionId ==. show sessionId] []

deleteHistoriesBySessionId :: T.Text -> SessionId -> IO ()
deleteHistoriesBySessionId schemaName sessionId = runSqlite schemaName $ deleteWhere [ChatHistorySessionId ==. show sessionId]

newtype SqliteChatMessageHistory = SqliteChatMessageHistory
  { schemaName :: T.Text
  }

migrate :: SqliteChatMessageHistory -> IO ()
migrate (SqliteChatMessageHistory schema) = migrate' schema

instance ChatMessageHistory SqliteChatMessageHistory where
  getMessages :: SqliteChatMessageHistory -> SessionId -> IO [ReqMessage]
  getMessages (SqliteChatMessageHistory schema) sessionId = do
    gotHistories <- getHistoriesBySessionId schema sessionId
    let histories = toDomain <$> gotHistories
    return $ message <$> catMaybes histories

  addMessage :: SqliteChatMessageHistory -> SessionId -> ReqMessage -> IO ()
  addMessage (SqliteChatMessageHistory schema) sessionId reqMessage = do
    let chatHistoryData = ChatHistoryData sessionId reqMessage
    let chatHistory = fromDomain chatHistoryData
    _ <- saveHistory schema chatHistory
    return ()

  deleteMessages :: SqliteChatMessageHistory -> SessionId -> IO ()
  deleteMessages (SqliteChatMessageHistory schema) = deleteHistoriesBySessionId schema
