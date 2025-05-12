{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Maybe (fromJust)
import GHC.IO.Handle (hFlush, hGetLine)
import System.Process (StdStream (CreatePipe), proc, std_in, std_out, withCreateProcess)

-- ping

ping :: Value
ping = object ["jsonrpc" .= ("2.0" :: String), "method" .= ("ping" :: String), "id" .= (1 :: Int)]

-- initialize

initialize :: Value
initialize =
  object
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

initialized :: Value
initialized =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("notifications/initialized" :: String)
    ]

-- prompts

listPrompts :: Value
listPrompts =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("prompts/list" :: String),
      "id" .= (1 :: Int),
      "params" .= object []
    ]

getPrompt :: Value
getPrompt =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("prompts/get" :: String),
      "id" .= (1 :: Int),
      "params" .= object ["name" .= ("list-vaults" :: String)]
    ]

-- resources

listResources :: Value
listResources =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("resources/list" :: String),
      "id" .= (1 :: Int),
      "params" .= object []
    ]

readResource :: Value
readResource =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("resources/read" :: String),
      "id" .= (1 :: Int),
      "params" .= object ["uri" .= ("obsidian-vault://obsidian" :: String)]
    ]

-- tools

listTools :: Value
listTools =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("tools/list" :: String),
      "id" .= (1 :: Int),
      "params" .= object []
    ]

readNote :: Value
readNote =
  object
    [ "jsonrpc" .= ("2.0" :: String),
      "method" .= ("tools/call" :: String),
      "id" .= (1 :: Int),
      "params"
        .= object
          [ "name" .= ("read-note" :: String),
            "arguments"
              .= object
                [ "vault" .= ("obsidian" :: String),
                  "filename" .= ("lgchain-hs.md" :: String)
                ]
          ]
    ]

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
