module Lgchain.Core.Clients where

import Lgchain.Core.Requests (FormatMap, JsonSchemaConvertable, Prompt)

data Output a where
  StrOutput :: String -> Output a
  StructedOutput :: (JsonSchemaConvertable a) => a -> Output a

strOutput :: Maybe (Output a) -> Maybe String
strOutput (Just (StrOutput str)) = Just str
strOutput _ = Nothing

structedOutput :: Maybe (Output a) -> Maybe a
structedOutput (Just (StructedOutput struct)) = Just struct
structedOutput _ = Nothing

deriving instance Show (Output a)

deriving instance Eq (Output a)

class LLMModel a where
  invokeWithSchema :: (JsonSchemaConvertable b) => a -> Prompt -> b -> Maybe FormatMap -> IO (Maybe (Output b))
  invokeStr :: a -> Prompt -> Maybe FormatMap -> IO (Maybe (Output b))

data Chain b a where
  Chain :: (JsonSchemaConvertable a, LLMModel b) => b -> Prompt -> a -> Chain b a
  StrChain :: (LLMModel b) => b -> Prompt -> Chain b a
