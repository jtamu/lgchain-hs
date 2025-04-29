{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Clients (Chain (Chain), ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), invoke, structedOutput)
import Data.Aeson (FromJSON)
import Data.Functor ((<&>))
import Data.Map qualified as M
import GHC.Generics (Generic)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (System, User))
import Requests (deriveJsonSchema)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recipe

deriveJsonSchema ''Recipe

main :: IO ()
main = do
  let prompt =
        [ ReqMessage System "ユーザが入力した料理のレシピを考えてください。また、日本語で回答してください。",
          ReqMessage User "{dish}"
        ]
  let model = ChatOpenAI GPT4O
  let chain = Chain model prompt (undefined :: Recipe)
  let formatMap = M.fromList [("{dish}", "カレー")]

  res <- invoke chain (Just formatMap) <&> structedOutput
  case res of
    Just recipe -> print recipe
    Nothing -> putStrLn "Failed to decode JSON"
