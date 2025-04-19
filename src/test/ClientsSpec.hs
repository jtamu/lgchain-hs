{-# LANGUAGE OverloadedStrings #-}

module ClientsSpec where

import Clients (Chain (StrChain), ChatOpenAI (ChatOpenAI), OpenAIModelName (GPT4O), buildReqBody)
import Data.Aeson (encode)
import Data.Map qualified as M
import Requests (ReqMessage (ReqMessage), Role (System, User))
import Test.Hspec (Spec, context, describe, it, shouldBe)

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
