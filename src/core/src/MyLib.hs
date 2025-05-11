{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Control.Concurrent (threadDelay)
import Data.Aeson (Value, encode, object, (.=))
import Data.ByteString.Lazy.Char8 (hPutStrLn)
import Data.Maybe (fromJust)
import GHC.IO.Handle (hFlush, hGetLine)
import System.Process (StdStream (CreatePipe), proc, std_in, std_out, withCreateProcess)

ping :: Value
ping = object ["jsonrpc" .= ("2.0" :: String), "method" .= ("ping" :: String), "id" .= (1 :: Int)]

someFunc :: IO ()
someFunc = withCreateProcess (proc "npx" ["-y", "obsidian-mcp", "/opt/app/docs/obsidian"]) {std_in = CreatePipe, std_out = CreatePipe} $
  \maybehin maybehout _ _ -> do
    let hin = fromJust maybehin
    let hout = fromJust maybehout

    hPutStrLn hin (encode ping)
    hFlush hin
    response1 <- hGetLine hout
    putStrLn $ "Response: " ++ response1

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode ping)
    hFlush hin
    response2 <- hGetLine hout
    putStrLn $ "Response: " ++ response2

    threadDelay 1000000 -- 1秒待機
    hPutStrLn hin (encode ping)
    hFlush hin
    response3 <- hGetLine hout
    putStrLn $ "Response: " ++ response3
