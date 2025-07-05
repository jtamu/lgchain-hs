{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Lgchain.OpenAI.Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (throwE)
import Data.Aeson (decode)
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Lgchain.Core.Clients (Chain (Chain, StrChain, ToolChain), ExceptIO, LLMModel (invokeStr, invokeWithSchema), LgchainError (ParsingError), Output (StrOutput, StructedOutput, ToolOutput), invokeTool)
import Lgchain.Core.Requests (FormatMap, FormatType (JsonFormat), JsonSchemaConvertable (convertJson), ReqBody (ReqBody), ResponseFormat (ResponseFormat), ViewableText, formatPrompt, vpack, vunpack)
import Lgchain.Core.Responses (ToolCall)
import Lgchain.OpenAI.Responses (ResBody, ResMessage (ResMessage), ResMessageContent (content), choices, toolCalls)
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import System.Environment (getEnv)

data OpenAIModelName = GPT4O | GPT3_5Turbo

instance Show OpenAIModelName where
  show GPT4O = "gpt-4o"
  show GPT3_5Turbo = "gpt-3.5-turbo"

newtype ChatOpenAI = ChatOpenAI {modelName :: OpenAIModelName} deriving (Show)

openAIModelNameStr :: ChatOpenAI -> String
openAIModelNameStr (ChatOpenAI modelName) = show modelName

extractStrOutput :: ResBody -> Maybe ViewableText
extractStrOutput resBody = case choices resBody of
  (ResMessage message : _) -> vpack <$> content message
  _ -> Nothing

extractStructedOutput :: (JsonSchemaConvertable a) => ResBody -> Maybe (Output a)
extractStructedOutput res = do
  str <- extractStrOutput res
  decoded <- decode $ LBS.pack $ UTF8.encode (vunpack str)
  return $ StructedOutput decoded

extractToolCalls :: ResBody -> Maybe [ToolCall]
extractToolCalls res = case choices res of
  (ResMessage message : _) -> toolCalls message
  _ -> Nothing

extractToolOutput :: ResBody -> Maybe (Output a)
extractToolOutput res = ToolOutput <$> extractToolCalls res

buildOutput :: Chain ChatOpenAI a -> ResBody -> Either LgchainError (Output a)
buildOutput (StrChain _ _) res = case extractStrOutput res of
  Just str -> Right $ StrOutput str
  Nothing -> Left $ ParsingError "Failed to extract string output from response"
buildOutput (Chain {}) res = case extractStructedOutput res of
  Just output -> Right output
  Nothing -> Left $ ParsingError "Failed to parse structured output from response"
buildOutput (ToolChain {}) res = case extractToolOutput res of
  Just output -> Right output
  Nothing ->
    case extractStrOutput res of
      Just str -> Right $ StrOutput str
      Nothing -> Left $ ParsingError "Failed to parse tool output from response"

instance LLMModel ChatOpenAI where
  invokeWithSchema model prompt schema maybeFormat =
    let chain = Chain model prompt schema in invokeOpenai chain maybeFormat
  invokeStr model prompt maybeFormat =
    let chain = StrChain model prompt in invokeOpenai chain maybeFormat
  invokeTool model prompt tools maybeFormat =
    let chain = ToolChain model prompt tools in invokeOpenai chain maybeFormat

buildReqBody :: Chain ChatOpenAI a -> Maybe FormatMap -> ReqBody
buildReqBody (Chain model prompt maybeData) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt (Just resFormat) Nothing
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat
    resFormat = ResponseFormat JsonFormat (convertJson maybeData)
buildReqBody (StrChain model prompt) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt Nothing Nothing
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat
buildReqBody (ToolChain model prompt tools) maybeFormat =
  ReqBody (openAIModelNameStr model) formattedPrompt Nothing (Just tools)
  where
    formattedPrompt = maybe prompt (`formatPrompt` prompt) maybeFormat

invokeOpenai :: Chain ChatOpenAI a -> Maybe FormatMap -> ExceptIO (Output a)
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
