{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Lgchain.Gemini.MyLib where

import Lgchain.Gemini.Clients (ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Lgchain.Core.Agents (AgentNode (run))
import Lgchain.Core.Clients (Chain (Chain, StrChain), ExceptIO, invoke, runOrFail, strOutput, structedOutput)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (System, User), ViewableText, deriveJsonSchema, vunpack)
import Text.RawString.QQ (r)

data ExampleState = ExampleState
  { recipe :: Recipe,
    ingredients :: [ViewableText]
  }
  deriving (Show, Generic)

data Recipe = Recipe
  { name :: ViewableText,
    recipeIngredients :: [ViewableText],
    steps :: [ViewableText]
  }
  deriving (Show, Eq, Generic)

deriveJsonSchema ''Recipe

instance AgentNode ExampleState String where
  run state input = do
    let chain =
          Chain
            (ChatGemini GEMINI_1_5_FLASH)
            [ ReqMessage
                System
                [r|
                  あなたは料理のレシピを考えるエキスパートです。
                  ユーザが入力した料理のレシピを考えてください。
                  また、日本語で回答してください。
                |],
              ReqMessage User "{dish}"
            ]
            (undefined :: Recipe)
    let formatMap = M.singleton "{dish}" input
    output <- invoke chain (Just formatMap)
    case structedOutput output of
      Right recipe -> do
        let newState = state {recipe = recipe}
        return newState
      Left err -> error $ show err

main :: IO ()
main = do
  let initialState = ExampleState (Recipe (fromJust $ viewable "カレー") [] []) []
  runOrFail $ do
    newState <- run initialState "カレー"
    liftIO $ print newState
