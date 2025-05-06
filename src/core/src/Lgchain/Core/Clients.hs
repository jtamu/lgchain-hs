module Lgchain.Core.Clients where

import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Except (ExceptT)
import Lgchain.Core.Requests (FormatMap, JsonSchemaConvertable, Prompt)

data Output a where
  StrOutput :: String -> Output a
  StructedOutput :: (JsonSchemaConvertable a) => a -> Output a

strOutput :: Output a -> Maybe String
strOutput (StrOutput str) = Just str
strOutput _ = Nothing

structedOutput :: Output a -> Maybe a
structedOutput (StructedOutput struct) = Just struct
structedOutput _ = Nothing

deriving instance Show (Output a)

deriving instance Eq (Output a)

data LgchainError
  = ApiError String         -- API呼び出し時のエラー
  | ParsingError String     -- レスポンスのパース時のエラー
  | AuthError String        -- 認証関連のエラー
  | ConfigError String      -- 設定関連のエラー
  | OtherError String       -- その他のエラー
  deriving (Show, Eq)

class LLMModel a where
  invokeWithSchema :: (JsonSchemaConvertable b) => a -> Prompt -> b -> Maybe FormatMap -> ExceptT LgchainError IO (Output b)
  invokeStr :: a -> Prompt -> Maybe FormatMap -> ExceptT LgchainError IO (Output b)

data Chain b a where
  Chain :: (JsonSchemaConvertable a, LLMModel b) => b -> Prompt -> a -> Chain b a
  StrChain :: (LLMModel b) => b -> Prompt -> Chain b a

invoke :: Chain b a -> Maybe FormatMap -> ExceptT LgchainError IO (Output a)
invoke (Chain model prompt struct) formatMap = invokeWithSchema model prompt struct formatMap
invoke (StrChain model prompt) formatMap = invokeStr model prompt formatMap
