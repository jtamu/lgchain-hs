# lgchain-hs

lgchain-hsは、大規模言語モデル（LLM）を使用するためのHaskellライブラリです。Python言語の[LangChain](https://github.com/langchain-ai/langchain)に着想を得ており、OpenAIのGPTモデルやGoogleのGeminiモデルなどのLLMを統一的なインターフェースで利用できます。

## 概要

lgchain-hsは以下の機能を提供します：

- 複数のLLMプロバイダー（OpenAI、Google Gemini）への統一インターフェース
- LLM操作のチェーン化と合成
- チャットメッセージ履歴の管理と永続化
- JSONスキーマを使用した構造化データの取得
- テンプレート変数を使用した動的プロンプトのフォーマット

## プロジェクト構成

このプロジェクトは以下の3つの主要コンポーネントで構成されています：

1. **コアライブラリ** (`lgchain-hs-core`): 基本的な抽象化とユーティリティを提供
2. **Gemini統合** (`lgchain-hs-gemini`): GoogleのGeminiモデルとの統合
3. **OpenAI統合** (`lgchain-hs-openai`): OpenAIのモデルとの統合

## インストール

### 前提条件

- GHC 9.4.8以上
- Cabal 3.0以上

### ビルド方法

リポジトリをクローンし、Cabalを使用してビルドします：

```bash
git clone https://github.com/jtamu/lgchain-hs.git
cd lgchain-hs
cabal build all
```

### Docker環境の使用

Docker環境を使用する場合：

```bash
cp .env.example .env
# .envファイルにAPIキーを設定
docker-compose up -d
```

## 使用方法

### 基本的な使い方

#### 1. 文字列出力の取得（StrChain）

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Lgchain.Core.Clients (Chain(StrChain), invoke, runOrFail, strOutput)
import Lgchain.Core.Requests (ReqMessage(ReqMessage), Role(System, User))
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Map qualified as M

-- OpenAIの場合
import Lgchain.OpenAI.Clients (ChatOpenAI(ChatOpenAI), OpenAIModelName(GPT4O))

main :: IO ()
main = runOrFail $ do
  let prompt = [
        ReqMessage System "You are a helpful assistant.",
        ReqMessage User "Haskellについて教えてください。"
      ]
  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt

  result <- invoke chain Nothing
  response <- ExceptT $ return $ strOutput result
  liftIO $ putStrLn response
```

#### 2. 構造化データの取得（Chain）

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Lgchain.Core.Clients (Chain(Chain), invoke, runOrFail, structedOutput)
import Lgchain.Core.Requests (ReqMessage(ReqMessage), Role(System, User), ViewableText, deriveJsonSchema)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Map qualified as M
import GHC.Generics (Generic)

-- Geminiの場合
import Lgchain.Gemini.Clients (ChatGemini(ChatGemini), GeminiModelName(GEMINI_1_5_FLASH))

-- 構造化データの定義
data Recipe = Recipe
  { ingredients :: [ViewableText],
    steps :: [ViewableText]
  }
  deriving (Eq, Show, Generic)

-- JSONスキーマの自動生成
deriveJsonSchema ''Recipe

main :: IO ()
main = runOrFail $ do
  let prompt = [
        ReqMessage System "ユーザが入力した料理のレシピを考えてください。",
        ReqMessage User "{dish}"
      ]
  let model = ChatGemini GEMINI_1_5_FLASH
  let chain = Chain model prompt (undefined :: Recipe)
  let formatMap = M.fromList [("{dish}", "カレー")]

  result <- invoke chain (Just formatMap)
  recipe <- ExceptT $ return $ structedOutput result
  liftIO $ print recipe
```

#### 3. チャット履歴の管理

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Lgchain.Core.Clients (Chain(StrChain), invoke, runOrFail, strOutput)
import Lgchain.Core.Histories.ChatMessageHistories (ChatMessageHistory(addMessage, deleteMessages), getMessages)
import Lgchain.Core.Histories.ChatMessageHistories.RDB (SqliteChatMessageHistory(SqliteChatMessageHistory), migrate)
import Lgchain.Core.Requests (ReqMessage(ReqMessage), Role(Assistant, System, User))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT))

-- OpenAIの場合
import Lgchain.OpenAI.Clients (ChatOpenAI(ChatOpenAI), OpenAIModelName(GPT4O))

main :: IO ()
main = runOrFail $ do
  -- 履歴の初期化
  let history = SqliteChatMessageHistory "database.db" "session1"
  liftIO $ migrate history

  -- 履歴へのメッセージ追加
  liftIO $ addMessage history (ReqMessage User "こんにちは")
  liftIO $ addMessage history (ReqMessage Assistant "こんにちは！どのようにお手伝いできますか？")

  -- 履歴の取得と利用
  messages <- liftIO $ getMessages history
  let userMessage = ReqMessage User "Haskellについて教えてください"
  let prompt = [ReqMessage System "You are a helpful assistant."] ++ messages ++ [userMessage]

  let model = ChatOpenAI GPT4O
  let chain = StrChain model prompt

  -- LLM呼び出しと履歴更新
  result <- invoke chain Nothing
  response <- ExceptT $ return $ strOutput result
  liftIO $ addMessage history userMessage
  liftIO $ addMessage history (ReqMessage Assistant response)

  -- 最終的な履歴の表示
  finalMessages <- liftIO $ getMessages history
  liftIO $ print finalMessages
```

### 高度な使用例

#### エージェントワークフロー

```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

import Lgchain.Core.Agents (AgentNode(run))
import Lgchain.Core.Clients (Chain(Chain, StrChain), invoke, runOrFail, strOutput, structedOutput)
import Lgchain.Core.Requests (ReqMessage(ReqMessage), Role(System, User), ViewableText, deriveJsonSchema)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT(ExceptT))
import Data.Map qualified as M
import GHC.Generics (Generic)
import Text.RawString.QQ (r)

-- Geminiの場合
import Lgchain.Gemini.Clients (ChatGemini(ChatGemini), GeminiModelName(GEMINI_1_5_FLASH))

-- エージェントの状態定義
data ExampleState = ExampleState
  { query :: ViewableText,
    currentRole :: Maybe ViewableText,
    messages :: [ViewableText]
  }
  deriving (Eq, Show, Generic)

-- ロール選択ノード
data SelectionNode = SelectionNode

instance AgentNode SelectionNode ExampleState where
  run _ state = do
    let prompt = [
          ReqMessage User [r|
            質問を分析し、最も適切な回答担当ロールを選択してください。
            選択肢：
            1.一般知識エキスパート
            2.技術エキスパート
            3.カウンセラー
            回答は選択肢の番号のみを返してください。
            質問：{query}
          |]
        ]
    let model = ChatGemini GEMINI_1_5_FLASH
    let chain = StrChain model prompt
    result <- invoke chain (Just $ M.singleton "{query}" (query state))
    roleNum <- ExceptT $ return $ strOutput result
    -- ロール選択ロジック（省略）
    return $ state { currentRole = Just "選択されたロール" }

-- 回答ノード
data AnsweringNode = AnsweringNode

instance AgentNode AnsweringNode ExampleState where
  run _ state = do
    -- 回答生成ロジック（省略）
    return $ state { messages = messages state ++ ["生成された回答"] }

-- ワークフロー実行
exampleWorkflow :: ExampleState -> ExceptIO ExampleState
exampleWorkflow state = do
  let selectionNode = SelectionNode
  let answeringNode = AnsweringNode

  -- ノードの連鎖実行
  finalState <- run selectionNode state >>= run answeringNode
  return finalState

main :: IO ()
main = runOrFail $ do
  state <- exampleWorkflow ExampleState
    { query = "Haskellについて教えてください",
      currentRole = Nothing,
      messages = []
    }
  liftIO $ print state
```

## API概要

### コアコンポーネント

#### LLMModel型クラス

LLMとの対話のための基本インターフェース：

```haskell
class LLMModel a where
  invokeWithSchema :: (JsonSchemaConvertable b) => a -> Prompt -> b -> Maybe FormatMap -> ExceptIO (Output b)
  invokeStr :: a -> Prompt -> Maybe FormatMap -> ExceptIO (Output b)
```

#### Chain型

LLM操作のチェーンを表現：

```haskell
data Chain b a where
  Chain :: (JsonSchemaConvertable a, LLMModel b) => b -> Prompt -> a -> Chain b a
  StrChain :: (LLMModel b) => b -> Prompt -> Chain b a
```

#### ChatMessageHistory型クラス

チャット履歴の管理インターフェース：

```haskell
class ChatMessageHistory a where
  getMessages :: a -> IO [ReqMessage]
  addMessage :: a -> ReqMessage -> IO ()
  deleteMessages :: a -> IO ()
```

### モデル実装

#### OpenAI

```haskell
data OpenAIModelName = GPT4O | GPT4 | GPT35Turbo
data ChatOpenAI = ChatOpenAI OpenAIModelName
```

#### Gemini

```haskell
data GeminiModelName = GEMINI_1_5_FLASH | GEMINI_1_5_PRO
data ChatGemini = ChatGemini GeminiModelName
```

## 環境変数

`.env`ファイルに以下の環境変数を設定します：

```
OPENAI_API_KEY=your_openai_api_key
GEMINI_API_KEY=your_gemini_api_key
```

## ライセンス

BSD-3-Clause

## 貢献

バグ報告や機能リクエストは[GitHub Issues](https://github.com/jtamu/lgchain-hs/issues)にお願いします。
プルリクエストも歓迎します。

## 開発ロードマップ

詳細な開発計画は[TODO.md](TODO.md)を参照してください。
