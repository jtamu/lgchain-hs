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
            "protocolVersion" .= ("0.1.0" :: String),
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
