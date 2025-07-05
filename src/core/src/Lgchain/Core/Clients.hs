{-# LANGUAGE OverloadedStrings #-}

module Lgchain.Core.Clients where

import Control.Monad.Except (runExceptT)
import Control.Monad.Trans.Except (ExceptT)
import Lgchain.Core.Requests (FormatMap, JsonSchemaConvertable, Prompt, Tool, ViewableText)
import Lgchain.Core.Responses (ToolCall)

data Output a where
  StrOutput :: ViewableText -> Output a
  StructedOutput :: (JsonSchemaConvertable a) => a -> Output a
  ToolOutput :: [ToolCall] -> Output a

data LgchainError
  = ApiError ViewableText -- API呼び出し時のエラー
  | ParsingError ViewableText -- レスポンスのパース時のエラー
  | AuthError ViewableText -- 認証関連のエラー
  | ConfigError ViewableText -- 設定関連のエラー
  | OtherError ViewableText -- その他のエラー
  deriving (Show, Eq)

type ExceptIO = ExceptT LgchainError IO

strOutput :: Output a -> Either LgchainError ViewableText
strOutput (StrOutput str) = Right str
strOutput _ = Left $ OtherError "Not ViewableText output"

structedOutput :: Output a -> Either LgchainError a
structedOutput (StructedOutput struct) = Right struct
structedOutput _ = Left $ OtherError "Not structed output"

toolOutput :: Output a -> Either LgchainError [ToolCall]
toolOutput (ToolOutput tool) = Right tool
toolOutput _ = Left $ OtherError "Not tool output"

deriving instance Show (Output a)

deriving instance Eq (Output a)

class LLMModel a where
  invokeWithSchema :: (JsonSchemaConvertable b) => a -> Prompt -> b -> Maybe FormatMap -> ExceptIO (Output b)
  invokeStr :: a -> Prompt -> Maybe FormatMap -> ExceptIO (Output b)
  invokeTool :: a -> Prompt -> [Tool] -> Maybe FormatMap -> ExceptIO (Output b)

data Chain b a where
  Chain :: (JsonSchemaConvertable a, LLMModel b) => b -> Prompt -> a -> Chain b a
  StrChain :: (LLMModel b) => b -> Prompt -> Chain b a
  ToolChain :: (LLMModel b) => b -> Prompt -> [Tool] -> Chain b a

invoke :: Chain b a -> Maybe FormatMap -> ExceptIO (Output a)
invoke (Chain model prompt struct) formatMap = invokeWithSchema model prompt struct formatMap
invoke (StrChain model prompt) formatMap = invokeStr model prompt formatMap
invoke (ToolChain model prompt tools) formatMap = invokeTool model prompt tools formatMap

runOrFail :: ExceptIO () -> IO ()
runOrFail action = do
  result <- runExceptT action
  case result of
    Left err -> putStrLn $ "Error: " ++ show err
    Right _ -> return ()
