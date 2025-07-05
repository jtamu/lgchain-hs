module Lgchain.Core.Responses where

import Data.Aeson (FromJSON, Value (Object, String), eitherDecode, parseJSON, withObject, (.:))
import Data.Aeson.Key (fromString, toText)
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as M
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import GHC.Generics (Generic)

data Function = Function
  { name :: String,
    arguments :: M.Map Text Value
  }
  deriving (Eq, Show, Generic)

instance FromJSON Function where
  parseJSON = withObject "Function" $ \v -> do
    functionName <- v .: fromString "name"
    rawArgs <- v .: fromString "arguments"
    args <- parseArguments rawArgs
    return $ Function functionName args

-- 引数をパースする補助関数
parseArguments :: Value -> Parser (M.Map Text Value)
parseArguments (Object obj) =
  -- KeyMapからMapへの変換
  return $ M.fromList [(toText k, v) | (k, v) <- KM.toList obj]
parseArguments (String s) =
  -- 文字列の場合はJSONとしてパースを試みる
  case eitherDecode (BL.fromStrict $ TE.encodeUtf8 s) of
    Right (Object obj) ->
      return $ M.fromList [(toText k, v) | (k, v) <- KM.toList obj]
    _ -> return M.empty
parseArguments _ = return M.empty

data ToolCall = ToolCall
  { toolCallId :: String,
    toolCallType :: String,
    toolCallFunction :: Function
  }
  deriving (Eq, Show, Generic)

instance FromJSON ToolCall where
  parseJSON = withObject "ToolCall" $ \v ->
    ToolCall
      <$> v .: fromString "id"
      <*> v .: fromString "type"
      <*> v .: fromString "function"
