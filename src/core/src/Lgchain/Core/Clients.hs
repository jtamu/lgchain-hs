module Lgchain.Core.Clients where

import Control.Monad.Trans.Maybe (MaybeT)
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

class LLMModel a where
  invokeWithSchema :: (JsonSchemaConvertable b) => a -> Prompt -> b -> Maybe FormatMap -> MaybeT IO (Output b)
  invokeStr :: a -> Prompt -> Maybe FormatMap -> MaybeT IO (Output b)

data Chain b a where
  Chain :: (JsonSchemaConvertable a, LLMModel b) => b -> Prompt -> a -> Chain b a
  StrChain :: (LLMModel b) => b -> Prompt -> Chain b a

invoke :: Chain b a -> Maybe FormatMap -> MaybeT IO (Output a)
invoke (Chain model prompt struct) formatMap = invokeWithSchema model prompt struct formatMap
invoke (StrChain model prompt) formatMap = invokeStr model prompt formatMap
