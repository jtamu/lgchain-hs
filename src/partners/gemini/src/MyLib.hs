{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module MyLib where

import Clients (ChatGemini (ChatGemini), GeminiModelName (GEMINI_1_5_FLASH))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Data.Functor (void, (<&>))
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Maybe (fromJust)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Lgchain.Core.Agents (AgentNode (run))
import Lgchain.Core.Clients (Chain (Chain, StrChain), invoke, strOutput, structedOutput, LgchainError(..))
import Lgchain.Core.Requests (ReqMessage (ReqMessage), Role (System, User), deriveJsonSchema)
import Text.RawString.QQ (r)

data ExampleState = ExampleState
  { query :: Text,
    currentRole :: Maybe String,
    messages :: [String],
    currentJudge :: Maybe Bool,
    judgementReason :: Maybe String
  }
  deriving (Eq, Show, Generic)

data SelectionNode = SelectionNode

instance AgentNode (MaybeT IO) SelectionNode ExampleState where
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
    res <- MaybeT $ do
      result <- runExceptT $ invoke chain (Just $ M.singleton "{query}" $ query state)
      return $ case result of
        Left _ -> Nothing
        Right output -> strOutput output
    let role
          | "1" `isPrefixOf` roleNum = Just "一般知識エキスパート"
          | "2" `isPrefixOf` roleNum = Just "生成AI製品エキスパート"
          | "3" `isPrefixOf` roleNum = Just "カウンセラー"
          | otherwise = Nothing
    return $ state {currentRole = role}

data AnsweringNode = AnsweringNode

instance AgentNode (MaybeT IO) AnsweringNode ExampleState where
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
    message <- MaybeT $ do
      result <- runExceptT $ invoke chain (Just $ M.fromList [("role", pack $ fromJust $ currentRole state), ("query", query state)])
      return $ case result of
        Left _ -> Nothing
        Right output -> strOutput output
    return $ state {messages = messages state ++ [message]}

data CheckNode = CheckNode

data Judgement = Judgement
  { judgement :: Bool,
    reason :: String
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Judgement

instance AgentNode (MaybeT IO) CheckNode ExampleState where
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
    judge <- MaybeT $ do
      result <- runExceptT $ invoke chain (Just $ M.fromList [("query", query state), ("answer", pack $ last $ messages state)])
      return $ case result of
        Left _ -> Nothing
        Right output -> structedOutput output
    return $ state {currentJudge = Just $ judgement judge, judgementReason = Just $ reason judge}

exampleWorkflow :: ExampleState -> MaybeT IO ExampleState
exampleWorkflow = exampleWorkflowWithCounter 0

-- | 再帰呼び出しの回数を制限するための補助関数
exampleWorkflowWithCounter :: Int -> ExampleState -> MaybeT IO ExampleState
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

execExampleWorkflow :: IO ExampleState
execExampleWorkflow = runMaybeT (exampleWorkflow initState) <&> fromJust
  where
    initState =
      ExampleState
        { query = "生成AIについて教えてください",
          currentRole = Nothing,
          messages = [],
          currentJudge = Nothing,
          judgementReason = Nothing
        }

data Recipe = Recipe
  { ingredients :: [String],
    steps :: [String]
  }
  deriving (Eq, Show, Generic)

deriveJsonSchema ''Recipe

someFunc :: IO ()
someFunc =
  void $
    runMaybeT
      ( do
          let prompt = [ReqMessage System "ユーザが入力した料理のレシピを考えてください。", ReqMessage User "{dish}"]
          let model = ChatGemini GEMINI_1_5_FLASH
          let chain = Chain model prompt (undefined :: Recipe)
          let formatMap = M.fromList [("{dish}", "カレー")]
          res <- MaybeT $ do
            result <- runExceptT $ invoke chain (Just formatMap)
            return $ case result of
              Left _ -> Nothing
              Right output -> structedOutput output
          liftIO $ print $ fromJust res
      )
