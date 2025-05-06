{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe (MaybeT (MaybeT))
import Control.Monad.Trans.Except (ExceptT(ExceptT), throwE)
import Data.Aeson (decode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Lgchain.Core.Clients (Chain (Chain, StrChain), LLMModel (invokeStr, invokeWithSchema), Output (StrOutput, StructedOutput), LgchainError(..))
import Lgchain.Core.Requests (FormatMap, FormatType (JsonFormat), JsonSchemaConvertable (convertJson), ReqBody (ReqBody), ResponseFormat (ResponseFormat), formatPrompt)
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import Responses (ResBody (choices), ResMessage (ResMessage), ResMessageContent (content))
import System.Environment (getEnv)

data OpenAIModelName = GPT4O | GPT3_5Turbo

instance Show OpenAIModelName where
  show GPT4O = "gpt-4o"
  show GPT3_5Turbo = "gpt-3.5-turbo"

newtype ChatOpenAI = ChatOpenAI {modelName :: OpenAIModelName} deriving (Show)

openAIModelNameStr :: ChatOpenAI -> String
openAIModelNameStr (ChatOpenAI modelName) = show modelName

extractStrOutput :: ResBody -> Maybe String
extractStrOutput resBody = case choices resBody of
  (ResMessage message : _) -> Just $ content message
  _ -> Nothing

extractStructedOutput :: (JsonSchemaConvertable a) => ResBody -> Maybe (Output a)
extractStructedOutput res = do
  str <- extractStrOutput res
  decoded <- decode $ LBS.pack $ UTF8.encode str
  return $ StructedOutput decoded

buildOutput :: Chain ChatOpenAI a -> ResBody -> Either LgchainError (Output a)
buildOutput (StrChain _ _) res = case extractStrOutput res of
  Just str -> Right $ StrOutput str
  Nothing -> Left $ ParsingError "Failed to extract string output from response"
buildOutput (Chain {}) res = case extractStructedOutput res of
  Just output -> Right output
  Nothing -> Left $ ParsingError "Failed to parse structured output from response"

instance LLMModel ChatOpenAI where
  invokeWithSchema model prompt schema maybeFormat =
    let chain = Chain model prompt schema in invokeOpenai chain maybeFormat
  invokeStr model prompt maybeFormat =
    let chain = StrChain model prompt in invokeOpenai chain maybeFormat

buildReqBody :: Chain ChatOpenAI a -> Maybe FormatMap -> ReqBody
buildReqBody (Chain model prompt maybeData) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt (Just resFormat)
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat
    resFormat = ResponseFormat JsonFormat (convertJson maybeData)
buildReqBody (StrChain model prompt) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt Nothing
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat



invokeOpenai :: Chain ChatOpenAI a -> Maybe FormatMap -> ExceptT LgchainError IO (Output a)
invokeOpenai chain formatMap = do
  openaiApiKey <- liftIO $ getEnv "OPENAI_API_KEY"
  let reqbody = buildReqBody chain formatMap
  let req =
        setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $
          setRequestBodyJSON reqbody $
            parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- liftIO $ do
    response <- httpJSON req
    return $ getResponseBody response
  case buildOutput chain res of
    Right output -> return output
    Left err -> throwE err
