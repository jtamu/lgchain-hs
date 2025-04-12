{-# LANGUAGE OverloadedStrings #-}

module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, strOutput)
import Data.Map qualified as M
import Requests (ReqMessage (ReqMessage), Role (System, User))

main :: IO ()
main = do
  let prompt = [ReqMessage System "語尾にニャーとつけてください", ReqMessage User "{subject}は好きですか？"]
  let model = ChatOpenAI GPT4O
  let formatMap = M.fromList [("{subject}", "猫")]
  res <- invoke model prompt formatMap
  let out = strOutput res
  case out of
    Just content -> putStrLn content
    Nothing -> putStrLn "No response received"
