{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Data.Aeson (decode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import Requests (JsonSchemaConvertable (convertJson), Prompt, ReqBody (ReqBody), ReqMessage (ReqMessage), ResponseFormat (ResponseFormat))
import Responses (ResBody (choices), ResMessage (ResMessage), ResMessageContent (content))
import System.Environment (getEnv)

data OpenAIModelName = GPT4O | GPT3_5Turbo

instance Show OpenAIModelName where
  show GPT4O = "gpt-4o"
  show GPT3_5Turbo = "gpt-3.5-turbo"

newtype ChatOpenAI = ChatOpenAI {modelName :: OpenAIModelName} deriving (Show)

type FormatMap = M.Map T.Text T.Text

formatAll :: T.Text -> FormatMap -> T.Text
formatAll = M.foldlWithKey (\acc k v -> T.replace k v acc)

formatPrompt :: FormatMap -> Prompt -> Prompt
formatPrompt formatMap prompt = [ReqMessage role (formatAll content formatMap) | ReqMessage role content <- prompt]

openAIModelNameStr :: ChatOpenAI -> String
openAIModelNameStr (ChatOpenAI modelName) = show modelName

strOutput :: ResBody -> Maybe String
strOutput resBody = case choices resBody of
  (ResMessage message : _) -> Just $ content message
  _ -> Nothing

structedOutput :: (JsonSchemaConvertable a) => ResBody -> Maybe a
structedOutput res = strOutput res >>= decode . LBS.pack . UTF8.encode

buildReqBody :: (JsonSchemaConvertable a) => ChatOpenAI -> Prompt -> Maybe FormatMap -> Maybe a -> ReqBody
buildReqBody model prompt maybeFormat maybeData = ReqBody (openAIModelNameStr model) formattedPrompt resFormat
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat
    resFormat = do
      dataSchema <- convertJson <$> maybeData
      return $ ResponseFormat "json_schema" dataSchema

invoke :: (JsonSchemaConvertable a) => ChatOpenAI -> Prompt -> Maybe FormatMap -> Maybe a -> IO ResBody
invoke model prompt formatMap resFormat = do
  openaiApiKey <- getEnv "OPENAI_API_KEY"
  let reqbody = buildReqBody model prompt formatMap resFormat
  let req = setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $ setRequestBodyJSON reqbody $ parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- httpJSON req
  return (getResponseBody res :: ResBody)
