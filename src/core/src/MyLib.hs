{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Control.Concurrent (threadDelay)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Maybe (fromJust)
import GHC.IO.Handle (hFlush, hGetLine)
import Lgchain.Core.MCP.Clients.Requests (Notification (Notification), Request (Request))
import System.Process (StdStream (CreatePipe), proc, std_in, std_out, withCreateProcess)

-- ping

ping :: Request
ping = Request "2.0" "ping" "1" Nothing

-- initialize

initialize :: Request
initialize =
  Request
    "2.0"
    "initialize"
    "1"
    ( Just
        ( object
            [ "jsonrpc" .= ("2.0" :: String),
              "method" .= ("initialize" :: String),
              "id" .= (1 :: Int),
              "params"
                .= object
                  [ "processId" .= (1 :: Int),
                    "clientInfo"
                      .= object
                        [ "name" .= ("obsidian-mcp" :: String),
                          "version" .= ("0.1.0" :: String)
                        ],
                    "rootUri" .= ("file:///opt/app/docs/obsidian" :: String),
                    "protocolVersion" .= ("2025-03-26" :: String),
                    "capabilities" .= object []
                  ]
            ]
        )
    )

initialized :: Notification
initialized = Notification "2.0" "notifications/initialized"

-- prompts

listPrompts :: Request
listPrompts =
  Request
    "2.0"
    "prompts/list"
    "1"
    Nothing

getPrompt :: Request
getPrompt =
  Request
    "2.0"
    "prompts/get"
    "1"
    (Just (object ["name" .= ("list-vaults" :: String)]))

-- resources

listResources :: Request
listResources =
  Request
    "2.0"
    "resources/list"
    "1"
    Nothing

readResource :: Request
readResource =
  Request
    "2.0"
    "resources/read"
    "1"
    (Just (object ["uri" .= ("obsidian-vault://obsidian" :: String)]))

listTools :: Request
listTools =
  Request
    "2.0"
    "tools/list"
    "1"
    Nothing

readNote :: Request
readNote =
  Request
    "2.0"
    "tools/call"
    "1"
    ( Just
        ( object
            [ "name" .= ("read-note" :: String),
              "arguments"
                .= object
                  [ "vault" .= ("obsidian" :: String),
                    "filename" .= ("lgchain-hs.md" :: String)
                  ]
            ]
        )
    )

someFunc :: IO ()
someFunc = withCreateProcess (proc "npx" ["-y", "obsidian-mcp", "/opt/app/docs/obsidian"]) {std_in = CreatePipe, std_out = CreatePipe} $
  \maybehin maybehout _ _ -> do
    let hin = fromJust maybehin
    let hout = fromJust maybehout

    hPutStrLn hin (encode ping)
    hFlush hin
    response1 <- hGetLine hout
    putStrLn $ "Request ping: " ++ show ping
    putStrLn $ "Response ping: " ++ response1

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode initialize)
    hFlush hin
    response2 <- hGetLine hout
    putStrLn $ "Request initialize: " ++ show initialize
    putStrLn $ "Response initialize: " ++ response2

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode initialized) -- initializedにはレスポンスが来ない
    hFlush hin
    putStrLn $ "Request initialized: " ++ show initialized

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode listPrompts)
    hFlush hin
    response3 <- hGetLine hout
    putStrLn $ "Request listPrompts: " ++ show listPrompts
    putStrLn $ "Response listPrompts: " ++ response3

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode getPrompt)
    hFlush hin
    response4 <- hGetLine hout
    putStrLn $ "Request getPrompt: " ++ show getPrompt
    putStrLn $ "Response getPrompt: " ++ response4

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode listResources)
    hFlush hin
    response5 <- hGetLine hout
    putStrLn $ "Request resourceList: " ++ show listResources
    putStrLn $ "Response resourceList: " ++ response5

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode readResource)
    hFlush hin
    response6 <- hGetLine hout
    putStrLn $ "Request readResource: " ++ show readResource
    putStrLn $ "Response readResource: " ++ response6

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode listTools)
    hFlush hin
    response7 <- hGetLine hout
    putStrLn $ "Request listTools: " ++ show listTools
    putStrLn $ "Response listTools: " ++ response7

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode readNote)
    hFlush hin
    response8 <- hGetLine hout
    putStrLn $ "Request readNote: " ++ show readNote
    putStrLn $ "Response readNote: " ++ response8
