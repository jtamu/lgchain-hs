{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Clients (Chain (StrChain), ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH), invoke)
import Data.Map qualified as M
import Requests (Content (Content, parts, role), GenerateContentRequest (GenerateContentRequest, contents, generationConfig), Part (Part, text), Role (User))
import Responses (extractResponseMessage)

someFunc :: IO ()
someFunc = do
  let req =
        GenerateContentRequest
          { contents =
              [ Content
                  { role = User,
                    parts = [Part {text = "語尾ににゃーとつけてください"}]
                  },
                Content
                  { role = User,
                    parts = [Part {text = "私の名前は{name}です"}]
                  }
              ],
            generationConfig = Nothing
          }
  let chain = StrChain (ChatGemini GEMINI_1_5_FLASH) req
  res <- invoke chain (Just $ M.fromList [("{name}", "田村")])
  print $ extractResponseMessage res
