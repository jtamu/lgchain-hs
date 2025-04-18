{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clients (Chain (Chain), ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, structedOutput)
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
  let chain = Chain model prompt (Just (undefined :: Recipe))
  res <- invoke chain Nothing <&> structedOutput
  case res of
    Just recipe -> print recipe
    Nothing -> putStrLn "Failed to decode JSON"
