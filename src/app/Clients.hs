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

data Output a = StrOutput String | StructedOutput a

extractStrOutput :: ResBody -> Maybe String
extractStrOutput resBody = case choices resBody of
  (ResMessage message : _) -> Just $ content message
  _ -> Nothing

strOutput :: Maybe (Output a) -> Maybe String
strOutput (Just (StrOutput str)) = Just str
strOutput _ = Nothing

extractStructedOutput :: (JsonSchemaConvertable a) => ResBody -> Maybe (Output a)
extractStructedOutput res = do
  str <- extractStrOutput res
  decoded <- decode $ LBS.pack $ UTF8.encode str
  return $ StructedOutput decoded

structedOutput :: Maybe (Output a) -> Maybe a
structedOutput (Just (StructedOutput struct)) = Just struct
structedOutput _ = Nothing

buildReqBody :: (JsonSchemaConvertable a) => ChatOpenAI -> Prompt -> Maybe FormatMap -> Maybe a -> ReqBody
buildReqBody model prompt maybeFormat maybeData = ReqBody (openAIModelNameStr model) formattedPrompt resFormat
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat
    resFormat = do
      dataSchema <- convertJson <$> maybeData
      return $ ResponseFormat "json_schema" dataSchema

invoke :: (JsonSchemaConvertable a) => ChatOpenAI -> Prompt -> Maybe FormatMap -> Maybe a -> IO (Maybe (Output a))
invoke model prompt formatMap resFormat = do
  openaiApiKey <- getEnv "OPENAI_API_KEY"
  let reqbody = buildReqBody model prompt formatMap resFormat
  let req =
        setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $
          setRequestBodyJSON reqbody $
            parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- httpJSON req
  let resBody = (getResponseBody res :: ResBody)
  case resFormat of
    Just _ -> return $ extractStructedOutput resBody
    Nothing -> return $ StrOutput <$> extractStrOutput resBody
