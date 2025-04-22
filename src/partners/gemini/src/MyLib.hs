{-# LANGUAGE OverloadedStrings #-}

module MyLib (someFunc) where

import Clients (invoke)
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
                    parts = [Part {text = "私の名前は田村です"}]
                  }
              ],
            generationConfig = Nothing
          }
  res <- invoke req
  print $ extractResponseMessage res
