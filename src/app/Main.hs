{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clients (Chain (StrChain), ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, strOutput)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import GHC.Generics (Generic)
import Requests (ReqMessage (ReqMessage), Role (System, User), deriveJsonSchema)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Recipe

deriveJsonSchema ''Recipe

main :: IO ()
main = do
  let prompt = [ReqMessage System "ユーザが入力した料理のレシピを考えてください。また、日本語で回答してください。", ReqMessage User "カレー"]
  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt
  res <- invoke chain Nothing <&> strOutput
  case res of
    Just recipe -> print recipe
    Nothing -> putStrLn "Failed to decode JSON"
