{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ClientsSpec where

import Lgchain.Gemini.Clients (ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH), buildOutput, buildReqBody)
import Codec.Binary.UTF8.String qualified as UTF8
import Data.Aeson (decode)
import Data.ByteString.Lazy qualified as BS
import Data.Map qualified as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Lgchain.Core.Clients (Chain (Chain, StrChain), strOutput, structedOutput)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (System, User), ViewableText, deriveJsonSchema)
import Lgchain.Gemini.Requests (GenerateContentRequest)
import Lgchain.Gemini.Responses (Candidate (Candidate), GenerateContentResponse (GenerateContentResponse))
import Lgchain.Gemini.Responses qualified as Res (Content (Content), Part (Part))
import Test.Hspec (Spec, context, describe, it, shouldBe)
import Text.RawString.QQ (r)

data Recipe = Recipe
  { ingredients :: [ViewableText],
    steps :: [ViewableText]
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Recipe

reqBodyFromStr :: String -> GenerateContentRequest
reqBodyFromStr = fromJust . decode . BS.pack . UTF8.encode

spec :: Spec
spec = describe "Clients" $ do
  describe "buildReqBody" $ do
    context "文字列出力の場合" $ do
      context "プロンプトフォーマットがない場合" $ do
        it "リクエストボディが正しいこと" $ do
          let chain =
                StrChain
                  (ChatGemini GEMINI_1_5_FLASH)
                  [ ReqMessage System "system message",
                    ReqMessage User "user message"
                  ]
          let reqbody = buildReqBody chain Nothing
          reqbody
            `shouldBe` reqBodyFromStr
              [r|
                  {
                    "contents": [
                      {
                        "parts": [
                          {"text": "system message"}
                        ],
                        "role": "user"
                      },
                      {
                        "parts": [
                          {"text": "user message"}
                        ],
                        "role": "user"
                      }
                    ],
                    "model": "gemini-1.5-flash"
                  }
              |]

      context "プロンプトフォーマットがある場合" $ do
        it "リクエストボディが正しいこと" $ do
          let chain =
                StrChain
                  (ChatGemini GEMINI_1_5_FLASH)
                  [ ReqMessage System "system message",
                    ReqMessage User "user message {hoge} {fuga}"
                  ]
          let formatMap = M.fromList [("{hoge}", "HOGE"), ("{fuga}", "FUGA")]
          let reqbody = buildReqBody chain (Just formatMap)
          reqbody
            `shouldBe` reqBodyFromStr
              [r|
                {
                  "contents": [
                    {
                      "parts": [
                        {"text": "system message"}
                      ],
                      "role": "user"
                    },
                    {
                      "parts": [
                        {"text": "user message HOGE FUGA"}
                      ],
                      "role": "user"
                    }
                  ],
                  "model": "gemini-1.5-flash"
                }
              |]

    context "構造化出力の場合" $ do
      context "プロンプトフォーマットがある場合" $ do
        it "リクエストボディが正しいこと" $ do
          let chain =
                Chain
                  (ChatGemini GEMINI_1_5_FLASH)
                  [ ReqMessage System "ユーザが入力した料理のレシピを考えてください。また、日本語で回答してください。",
                    ReqMessage User "{dish}"
                  ]
                  (undefined :: Recipe)
          let reqbody = buildReqBody chain (Just $ M.singleton "{dish}" "カレー")
          reqbody
            `shouldBe` reqBodyFromStr
              [r|
                {
                  "contents": [
                    {
                      "parts": [
                        {"text": "ユーザが入力した料理のレシピを考えてください。また、日本語で回答してください。"}
                      ],
                      "role": "user"
                    },
                    {
                      "parts": [
                        {"text": "カレー"}
                      ],
                      "role": "user"
                    }
                  ],
                  "generationConfig": {
                    "response_mime_type": "application/json",
                    "response_schema": {
                      "properties": {
                        "ingredients": {
                          "items": {
                            "properties": {},
                            "required": [],
                            "type": "string"
                          },
                          "properties": {},
                          "required": [],
                          "type": "array"
                        },
                        "steps": {
                          "items": {
                            "properties": {},
                            "required": [],
                            "type": "string"
                          },
                          "properties": {},
                          "required": [],
                          "type": "array"
                        }
                      },
                      "required": [
                        "ingredients",
                        "steps"
                      ],
                      "type": "object"
                    }
                  }
                }
              |]

  describe "buildOutput" $ do
    context "文字列出力の場合" $ do
      it "出力が正しいこと" $ do
        let chain =
              StrChain
                (ChatGemini GEMINI_1_5_FLASH)
                [ ReqMessage System "system message",
                  ReqMessage User "user message"
                ]
        let resBody = GenerateContentResponse [Candidate (Res.Content [Res.Part "response message"] "assistant")]
        let output = buildOutput chain resBody
        (strOutput =<< output) `shouldBe` Right "response message"

    context "構造化出力の場合" $ do
      it "出力が正しいこと" $ do
        let chain =
              Chain
                (ChatGemini GEMINI_1_5_FLASH)
                [ ReqMessage System "system message",
                  ReqMessage User "user message"
                ]
                (undefined :: Recipe)
        let resBody =
              GenerateContentResponse
                [ Candidate
                    ( Res.Content
                        [ Res.Part
                            [r|
                              {
                                "ingredients": ["ing1", "ing2"],
                                "steps": ["step1", "step2"]
                              }
                            |]
                        ]
                        "assistant"
                    )
                ]
        let output = buildOutput chain resBody
        (structedOutput =<< output) `shouldBe` Right (Recipe ["ing1", "ing2"] ["step1", "step2"])
