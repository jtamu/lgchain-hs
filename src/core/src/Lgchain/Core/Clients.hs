module Lgchain.Core.Clients where

import Control.Monad.Trans.Except (ExceptT)
import Lgchain.Core.Requests (FormatMap, JsonSchemaConvertable, Prompt)

data Output a where
  StrOutput :: String -> Output a
  StructedOutput :: (JsonSchemaConvertable a) => a -> Output a

data LgchainError
  = ApiError String -- API呼び出し時のエラー
  | ParsingError String -- レスポンスのパース時のエラー
  | AuthError String -- 認証関連のエラー
  | ConfigError String -- 設定関連のエラー
  | OtherError String -- その他のエラー
  deriving (Show, Eq)

type ExceptIO = ExceptT LgchainError IO

strOutput :: Output a -> Either LgchainError String
strOutput (StrOutput str) = Right str
strOutput _ = Left $ OtherError "Not string output"

structedOutput :: Output a -> Either LgchainError a
structedOutput (StructedOutput struct) = Right struct
structedOutput _ = Left $ OtherError "Not structed output"

deriving instance Show (Output a)

deriving instance Eq (Output a)

class LLMModel a where
  invokeWithSchema :: (JsonSchemaConvertable b) => a -> Prompt -> b -> Maybe FormatMap -> ExceptIO (Output b)
  invokeStr :: a -> Prompt -> Maybe FormatMap -> ExceptIO (Output b)

data Chain b a where
  Chain :: (JsonSchemaConvertable a, LLMModel b) => b -> Prompt -> a -> Chain b a
  StrChain :: (LLMModel b) => b -> Prompt -> Chain b a

invoke :: Chain b a -> Maybe FormatMap -> ExceptIO (Output a)
invoke (Chain model prompt struct) formatMap = invokeWithSchema model prompt struct formatMap
invoke (StrChain model prompt) formatMap = invokeStr model prompt formatMap
