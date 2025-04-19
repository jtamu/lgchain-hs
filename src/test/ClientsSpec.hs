{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module ClientsSpec where

import Clients (Chain (Chain, StrChain), ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), buildReqBody)
import Data.Aeson (FromJSON, encode)
import Data.Map qualified as M
import GHC.Generics (Generic)
import Requests (ReqMessage (ReqMessage), Role (System, User), deriveJsonSchema)
import Test.Hspec (Spec, context, describe, it, shouldBe)

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Show, Generic)

instance FromJSON Recipe

deriveJsonSchema ''Recipe

spec :: Spec
spec = describe "buildReqBody" $ do
  context "文字列出力の場合" $ do
    context "プロンプトフォーマットがない場合" $ do
      it "リクエストボディが正しいこと" $ do
        let chain =
              StrChain
                (ChatOpenAI GPT4O)
                [ ReqMessage System "system message",
                  ReqMessage User "user message"
                ]
        let reqbody = buildReqBody chain Nothing
        encode reqbody
          `shouldBe` "{\"messages\":[{\"content\":\"system message\",\"role\":\"system\"},{\"content\":\"user message\",\"role\":\"user\"}],\"model\":\"gpt-4o\"}"

    context "プロンプトフォーマットがある場合" $ do
      it "リクエストボディが正しいこと" $ do
        let chain =
              StrChain
                (ChatOpenAI GPT4O)
                [ ReqMessage System "system message",
                  ReqMessage User "user message {hoge} {fuga}"
                ]
        let formatMap = M.fromList [("{hoge}", "HOGE"), ("{fuga}", "FUGA")]
        let reqbody = buildReqBody chain (Just formatMap)
        encode reqbody
          `shouldBe` "{\"messages\":[{\"content\":\"system message\",\"role\":\"system\"},{\"content\":\"user message HOGE FUGA\",\"role\":\"user\"}],\"model\":\"gpt-4o\"}"

  context "構造化出力の場合" $ do
    context "プロンプトフォーマットがない場合" $ do
      it "リクエストボディが正しいこと" $ do
        let chain =
              Chain
                (ChatOpenAI GPT4O)
                [ ReqMessage System "system message",
                  ReqMessage User "user message"
                ]
                (undefined :: Recipe)
        let reqbody = buildReqBody chain Nothing
        encode reqbody
          `shouldBe` "{\"messages\":[{\"content\":\"system message\",\"role\":\"system\"},{\"content\":\"user message\",\"role\":\"user\"}],\"model\":\"gpt-4o\",\"response_format\":{\"json_schema\":{\"name\":\"Recipe\",\"schema\":{\"properties\":{\"ingredients\":{\"items\":{\"type\":\"string\"},\"type\":\"array\",\"unique_items\":true},\"steps\":{\"items\":{\"type\":\"string\"},\"type\":\"array\",\"unique_items\":true}},\"required\":[\"ingredients\",\"steps\"],\"type\":\"object\"}},\"type\":\"json_schema\"}}"
