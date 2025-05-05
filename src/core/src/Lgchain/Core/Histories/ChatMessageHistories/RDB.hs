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
import Lgchain.Core.Histories.ChatMessageHistories (ChatMessageHistory (deleteMessages), SessionId, addMessage, getMessages)
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

type SchemaName = T.Text

migrate' :: SchemaName -> IO ()
migrate' schemaName = runSqlite schemaName (runMigration migrateAll)

fromDomain :: SessionId -> ReqMessage -> ChatHistory
fromDomain sessionId reqMessage =
  ChatHistory sessionId (show $ role reqMessage) (T.unpack $ content reqMessage)

toDomain :: Entity ChatHistory -> Maybe ReqMessage
toDomain (Entity _ history) = do
  readRole <- readMaybe (chatHistoryRole history)
  return $ ReqMessage readRole $ T.pack $ chatHistoryContent history

saveHistory :: SchemaName -> ChatHistory -> IO (Key ChatHistory)
saveHistory schemaName history = runSqlite schemaName $ insert history

getHistoriesBySessionId :: SchemaName -> SessionId -> IO [Entity ChatHistory]
getHistoriesBySessionId schemaName sessionId = runSqlite schemaName $ selectList [ChatHistorySessionId ==. sessionId] []

deleteHistoriesBySessionId :: SchemaName -> SessionId -> IO ()
deleteHistoriesBySessionId schemaName sessionId = runSqlite schemaName $ deleteWhere [ChatHistorySessionId ==. sessionId]

data SqliteChatMessageHistory = SqliteChatMessageHistory
  { schemaName :: SchemaName,
    sessionId :: SessionId
  }

migrate :: SqliteChatMessageHistory -> IO ()
migrate (SqliteChatMessageHistory schema _) = migrate' schema

instance ChatMessageHistory SqliteChatMessageHistory where
  getMessages :: SqliteChatMessageHistory -> IO [ReqMessage]
  getMessages (SqliteChatMessageHistory schema sessionId) = do
    gotHistories <- getHistoriesBySessionId schema sessionId
    let histories = toDomain <$> gotHistories
    return $ catMaybes histories

  addMessage :: SqliteChatMessageHistory -> ReqMessage -> IO ()
  addMessage (SqliteChatMessageHistory schema sessionId) reqMessage = do
    let chatHistory = fromDomain sessionId reqMessage
    _ <- saveHistory schema chatHistory
    return ()

  deleteMessages :: SqliteChatMessageHistory -> IO ()
  deleteMessages (SqliteChatMessageHistory schema sessionId) = deleteHistoriesBySessionId schema sessionId
