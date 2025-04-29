{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Data.Aeson (decode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Lgchain.Core.Requests (FormatMap, Prompt, formatPrompt)
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import Requests (FormatType (JsonFormat), JsonSchemaConvertable (convertJson), ReqBody (ReqBody), ResponseFormat (ResponseFormat))
import Responses (ResBody (choices), ResMessage (ResMessage), ResMessageContent (content))
import System.Environment (getEnv)

data OpenAIModelName = GPT4O | GPT3_5Turbo

instance Show OpenAIModelName where
  show GPT4O = "gpt-4o"
  show GPT3_5Turbo = "gpt-3.5-turbo"

newtype ChatOpenAI = ChatOpenAI {modelName :: OpenAIModelName} deriving (Show)

openAIModelNameStr :: ChatOpenAI -> String
openAIModelNameStr (ChatOpenAI modelName) = show modelName

data Output a where
  StrOutput :: String -> Output a
  StructedOutput :: (JsonSchemaConvertable a) => a -> Output a

deriving instance Show (Output a)

deriving instance Eq (Output a)

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

data Chain a where
  Chain :: (JsonSchemaConvertable a) => ChatOpenAI -> Prompt -> a -> Chain a
  StrChain :: ChatOpenAI -> Prompt -> Chain a

buildReqBody :: Chain a -> Maybe FormatMap -> ReqBody
buildReqBody (Chain model prompt maybeData) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt (Just resFormat)
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat
    resFormat = ResponseFormat JsonFormat (convertJson maybeData)
buildReqBody (StrChain model prompt) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt Nothing
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat

buildOutput :: Chain a -> ResBody -> Maybe (Output a)
buildOutput (StrChain _ _) res = StrOutput <$> extractStrOutput res
buildOutput (Chain {}) res = extractStructedOutput res

invoke :: Chain a -> Maybe FormatMap -> IO (Maybe (Output a))
invoke chain formatMap = do
  openaiApiKey <- getEnv "OPENAI_API_KEY"
  let reqbody = buildReqBody chain formatMap
  let req =
        setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $
          setRequestBodyJSON reqbody $
            parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- httpJSON req
  let resBody = getResponseBody res
  return $ buildOutput chain resBody
