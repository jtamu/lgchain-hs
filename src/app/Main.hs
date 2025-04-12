{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, strOutput)
import Data.Map qualified as M
import Requests (ReqMessage (ReqMessage), Role (System, User), sampleResFormat)

main :: IO ()
main = do
  let prompt = [ReqMessage System "ユーザが入力した料理のレシピを考えてください。また、日本語で回答してください。", ReqMessage User "カレー"]
  let model = ChatOpenAI GPT4O
  let formatMap = M.fromList [("{subject}", "猫")]
  res <- invoke model prompt formatMap $ Just sampleResFormat
  let out = strOutput res
  case out of
    Just content -> putStrLn content
    Nothing -> putStrLn "No response received"
