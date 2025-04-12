module Main where

import Clients (ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, strOutput)
import Requests (ReqMessage (ReqMessage))

main :: IO ()
main = do
  let prompt = [ReqMessage "system" "語尾にニャーとつけてください", ReqMessage "user" "猫は好きですか？"]
  let model = ChatOpenAI GPT4O
  res <- invoke model prompt
  let out = strOutput res
  case out of
    Just content -> putStrLn content
    Nothing -> putStrLn "No response received"
