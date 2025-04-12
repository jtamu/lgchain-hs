{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, strOutput)
import Codec.Binary.UTF8.String qualified as UTF8
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy qualified as BS
import GHC.Generics (Generic)
import Requests (ReqMessage (ReqMessage), Role (System, User), sampleResFormat)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Recipe

main :: IO ()
main = do
  let prompt = [ReqMessage System "ユーザが入力した料理のレシピを考えてください。また、日本語で回答してください。", ReqMessage User "カレー"]
  let model = ChatOpenAI GPT4O
  res <- invoke model prompt Nothing $ Just sampleResFormat
  let out = strOutput res
  case out of
    Just content ->
      let decoded = decode (BS.pack (UTF8.encode content)) :: Maybe Recipe
       in case decoded of
            Just recipe -> print recipe
            Nothing -> putStrLn "Failed to decode JSON"
    Nothing -> putStrLn "No response received"
