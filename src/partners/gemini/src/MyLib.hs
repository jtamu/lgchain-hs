{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Lgchain.Core.Agents (AgentNode (run))
import Lgchain.Core.Clients (Chain (Chain, StrChain), ExceptIO, invoke, runOrFail, strOutput, structedOutput)
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (System, User), ViewableText, deriveJsonSchema, vunpack)
import Lgchain.Gemini.Clients (ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH))
import Text.RawString.QQ (r)

data ExampleState = ExampleState
  { query :: ViewableText,
    currentRole :: Maybe ViewableText,
    messages :: [ViewableText],
    currentJudge :: Maybe Bool,
    judgementReason :: Maybe ViewableText
  }
  deriving (Eq, Show, Generic)

data SelectionNode = SelectionNode

instance AgentNode SelectionNode ExampleState where
  run _ state = do
    let prompt =
          [ ReqMessage
              User
              [r|
                質問を分析し、最も適切な回答担当ロールを選択してください。

                選択肢：
                1.一般知識エキスパート: 一般的な知識を持つエキスパート
                2.生成AI製品エキスパート: 生成AI製品を開発するエキスパート
                3.カウンセラー: 心理的な問題や不安を解消するカウンセラー

                回答は選択肢の番号(1,2または3)のみを返してください。

                質問：{query}
              |]
          ]
    let model = ChatGemini GEMINI_1_5_FLASH
    let chain = StrChain model prompt
    result <- invoke chain (Just $ M.singleton "{query}" (query state))
    roleNum <- ExceptT $ return $ strOutput result
    let role
          | "1" `isPrefixOf` vunpack roleNum = Just "一般知識エキスパート"
          | "2" `isPrefixOf` vunpack roleNum = Just "生成AI製品エキスパート"
          | "3" `isPrefixOf` vunpack roleNum = Just "カウンセラー"
          | otherwise = Nothing
    return $ state {currentRole = role}

data AnsweringNode = AnsweringNode

instance AgentNode AnsweringNode ExampleState where
  run _ state = do
    let prompt =
          [ ReqMessage
              User
              [r|
                あなたは{role}として回答してください。以下の質問に対して、あなたの役割に基づいた適切な回答を提供してください。

                役割の詳細：
                - 一般知識エキスパート: 幅広い分野の一般的な質問に対して、正確でわかりやすい回答を提供してください。
                - 生成AI製品エキスパート: 生成AIや関連製品、技術に関する専門的な質問に対して、最新の情報と深い洞察を提供してください。
                - カウンセラー: 心理的な問題や感情に関する質問に対して、共感と理解を持って対応し、前向きなアドバイスを提供してください。

                質問：{query}

                回答:
              |]
          ]
    let model = ChatGemini GEMINI_1_5_FLASH
    let chain = StrChain model prompt
    result <- invoke chain (Just $ M.fromList [("role", fromJust $ currentRole state), ("query", query state)])
    message <- ExceptT $ return $ strOutput result
    return $ state {messages = messages state ++ [message]}

data CheckNode = CheckNode

data Judgement = Judgement
  { judgement :: Bool,
    reason :: ViewableText
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Judgement

instance AgentNode CheckNode ExampleState where
  run _ state = do
    let prompt =
          [ ReqMessage
              User
              [r|
                以下の回答の品質をチェックし、問題がある場合は'false'、問題がない場合は'true'を返してください。また、その判定理由も説明してください。

                ユーザからの質問：{query}
                回答：{answer}
              |]
          ]
    let model = ChatGemini GEMINI_1_5_FLASH
    let chain = Chain model prompt (undefined :: Judgement)
    result <- invoke chain (Just $ M.fromList [("query", query state), ("answer", last $ messages state)])
    judge <- ExceptT $ return $ structedOutput result
    return $ state {currentJudge = Just $ judgement judge, judgementReason = Just $ reason judge}

exampleWorkflow :: ExampleState -> ExceptIO ExampleState
exampleWorkflow = exampleWorkflowWithCounter 0

-- | 再帰呼び出しの回数を制限するための補助関数
exampleWorkflowWithCounter :: Int -> ExampleState -> ExceptIO ExampleState
exampleWorkflowWithCounter counter state
  | counter >= 3 = return state -- 3回以上の繰り返しで強制終了
  | otherwise = do
      let selectionNode = SelectionNode
      let answeringNode = AnsweringNode
      let checkNode = CheckNode

      checkedState <- run selectionNode state >>= run answeringNode >>= run checkNode
      case currentJudge checkedState of
        Just True -> return checkedState
        _ -> exampleWorkflowWithCounter (counter + 1) checkedState

execExampleWorkflow :: IO ()
execExampleWorkflow = runOrFail $ do
  state <-
    exampleWorkflow
      ExampleState
        { query = "生成AIについて教えてください",
          currentRole = Nothing,
          messages = [],
          currentJudge = Nothing,
          judgementReason = Nothing
        }
  liftIO $ print state

data Recipe = Recipe
  { ingredients :: [ViewableText],
    steps :: [ViewableText]
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Recipe

someFunc :: IO ()
someFunc = runOrFail $ do
  let prompt = [ReqMessage System "ユーザが入力した料理のレシピを考えてください。", ReqMessage User "{dish}"]
  let model = ChatGemini GEMINI_1_5_FLASH
  let chain = Chain model prompt (undefined :: Recipe)
  let formatMap = M.fromList [("{dish}", "カレー")]
  result <- invoke chain (Just formatMap)
  res <- ExceptT $ return $ structedOutput result
  liftIO $ print res
