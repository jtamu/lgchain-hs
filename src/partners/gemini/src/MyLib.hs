{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib where

import Clients (ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH))
import Data.Aeson (FromJSON)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Lgchain.Core.Clients (Chain (Chain), invoke, structedOutput)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (System, User), deriveJsonSchema)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recipe

deriveJsonSchema ''Recipe

someFunc :: IO ()
someFunc = do
  let prompt = [ReqMessage System "ユーザが入力した料理のレシピを考えてください。", ReqMessage User "{dish}"]
  let model = ChatGemini GEMINI_1_5_FLASH
  let chain = Chain model prompt (undefined :: Recipe)
  let formatMap = M.fromList [("{dish}", "カレー")]
  res <- invoke chain (Just formatMap)
  print $ structedOutput res
