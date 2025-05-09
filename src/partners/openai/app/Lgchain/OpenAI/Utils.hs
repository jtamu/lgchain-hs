module Lgchain.OpenAI.Utils where

import Data.List.Split (splitOn)

takeAfterLastDot :: String -> String
takeAfterLastDot s = last (splitOn "." s)
