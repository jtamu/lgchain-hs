{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Clients (Chain (StrChain), ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH), ReqMessage (ReqMessage), invoke, strOutput)
import Data.Map qualified as M
import Requests (Role (User))

someFunc :: IO ()
someFunc = do
  let prompt = [ReqMessage User "語尾ににゃーとつけてください", ReqMessage User "私の名前は{name}です"]
  let model = ChatGemini GEMINI_1_5_FLASH
  let chain = StrChain model prompt
  let formatMap = M.fromList [("{name}", "田村")]
  res <- invoke chain (Just formatMap)
  print $ strOutput res
