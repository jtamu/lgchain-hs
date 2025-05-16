{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.MCP.Clients where

import Control.Monad.Except (ExceptT (ExceptT))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON (toJSON), eitherDecode, encode)
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.ByteString.Lazy.UTF8 qualified as BU
import Data.Maybe (fromJust)
import GHC.IO.Handle (hFlush, hGetLine)
import Lgchain.Core.Clients
  ( ExceptIO,
    LgchainError (ParsingError),
  )
import Lgchain.Core.MCP.Clients.Requests (Request (Request), ToolCallParams)
import Lgchain.Core.MCP.Clients.Responses (ContentItem, Response, Tool, ToolCallResult, ToolsListResult, errorResponseToLgchainError, getContentItemsFromSuccessResponse, getResponse, getToolsFromSuccessResponse)
import Lgchain.Core.Requests (vpack)
import System.IO (Handle)
import System.Process (CreateProcess (std_in), StdStream (CreatePipe), proc, std_out, withCreateProcess)

listToolsReq :: Request
listToolsReq =
  Request
    "2.0"
    "tools/list"
    "1"
    Nothing

callToolReq :: ToolCallParams -> Request
callToolReq params =
  Request
    "2.0"
    "tools/call"
    "1"
    (Just $ toJSON params)

class MCPClient a where
  withMCPConnection :: (a -> IO ()) -> IO ()

  listTools :: a -> ExceptIO [Tool]

  callTool :: a -> ToolCallParams -> ExceptIO [ContentItem]

data StdioMCPClient = StdioMCPClient
  { stdin :: Handle,
    stdout :: Handle
  }

instance MCPClient StdioMCPClient where
  withMCPConnection f = withCreateProcess (proc "npx" ["-y", "obsidian-mcp", "/opt/app/docs/obsidian"]) {std_in = CreatePipe, std_out = CreatePipe} $
    \maybehin maybehout _ _ -> do
      let hin = fromJust maybehin
      let hout = fromJust maybehout
      f StdioMCPClient {stdin = hin, stdout = hout}

  listTools client = do
    liftIO $ hPutStrLn (stdin client) (encode listToolsReq)
    liftIO $ hFlush (stdin client)
    response <- liftIO $ hGetLine (stdout client)
    let responseJson = eitherDecode $ BU.fromString response :: Either String (Response ToolsListResult)
    toolsListResult <- ExceptT $ return $ either (Left . ParsingError . vpack) (Right . getResponse) responseJson
    ExceptT $ return $ either (Left . errorResponseToLgchainError) (Right . getToolsFromSuccessResponse) toolsListResult

  callTool client params = do
    liftIO $ hPutStrLn (stdin client) (encode $ callToolReq params)
    liftIO $ hFlush (stdin client)
    response <- liftIO $ hGetLine (stdout client)
    let responseJson = eitherDecode $ BU.fromString response :: Either String (Response ToolCallResult)
    toolCallResult <- ExceptT $ return $ either (Left . ParsingError . vpack) (Right . getResponse) responseJson
    ExceptT $ return $ either (Left . errorResponseToLgchainError) (Right . getContentItemsFromSuccessResponse) toolCallResult
