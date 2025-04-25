{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib (someFunc) where

import Clients (Chain (Chain), ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH), ReqMessage (ReqMessage), invoke, structedOputput)
import Data.Aeson (FromJSON)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Requests (Role (User), deriveJSONSchema)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Eq, Show, Generic)

instance FromJSON Recipe

deriveJSONSchema ''Recipe

someFunc :: IO ()
someFunc = do
  let prompt = [ReqMessage User "ユーザが入力した料理のレシピを考えてください。", ReqMessage User "{dish}"]
  let model = ChatGemini GEMINI_1_5_FLASH
  let chain = Chain model prompt (undefined :: Recipe)
  let formatMap = M.fromList [("{dish}", "カレー")]
  res <- invoke chain (Just formatMap)
  print $ structedOputput res
