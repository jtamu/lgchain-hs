{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Data.ByteString qualified as BS
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (hContentType)
import Requests (GenerateContentRequest)
import Responses (GenerateContentResponse)
import System.Environment (getEnv)

invoke :: GenerateContentRequest -> IO GenerateContentResponse
invoke reqBody = do
  openaiApiKey <- getEnv "GEMINI_API_KEY"
  let req =
        setRequestHeaders [(hContentType, "application/json")] $
          setRequestBodyJSON reqBody $
            setRequestQueryString [("key", Just $ BS.pack $ UTF8.encode openaiApiKey)] $
              parseRequest_ "POST https://generativelanguage.googleapis.com/v1beta/models/gemini-1.5-flash:generateContent"
  res <- httpJSON req
  return $ getResponseBody res
