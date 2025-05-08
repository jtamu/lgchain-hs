{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, throwE)
import Data.Aeson (decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Lgchain.Core.Clients (Chain (Chain, StrChain), LLMModel (invokeStr, invokeWithSchema), LgchainError (ParsingError), Output (StrOutput, StructedOutput))
import Lgchain.Core.Requests (FormatMap, JsonSchemaConvertable (convertJson), ReqMessage (ReqMessage), ViewableText, formatPrompt, unviewable, viewable)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (hContentType)
import Requests (Content (Content), GenerateContentRequest (GenerateContentRequest), Part (Part), Role (Role), mapCommonSchemaDefinition)
import Responses (Content (parts), GenerateContentResponse (GenerateContentResponse), Part (text))
import Responses qualified as Res (Candidate (content))
import System.Environment (getEnv)

data GeminiModelName = GEMINI_1_5_FLASH

instance Show GeminiModelName where
  show GEMINI_1_5_FLASH = "gemini-1.5-flash"

newtype ChatGemini = ChatGemini {modelName :: GeminiModelName} deriving (Show)

geminiModelNameStr :: ChatGemini -> String
geminiModelNameStr (ChatGemini modelName) = show modelName

extractStrOutput :: GenerateContentResponse -> Maybe ViewableText
extractStrOutput (GenerateContentResponse candidates) =
  let messages = [text part | candidate <- candidates, part <- parts $ Res.content candidate]
   in case messages of
        (message : _) -> Just $ viewable message
        _ -> Nothing

extractStructedOutput :: (JsonSchemaConvertable a) => GenerateContentResponse -> Maybe (Output a)
extractStructedOutput res = do
  str <- extractStrOutput res
  decoded <- decode $ LBS.pack $ UTF8.encode (T.unpack $ unviewable str)
  return $ StructedOutput decoded

buildReqBody :: Chain b a -> Maybe FormatMap -> GenerateContentRequest
buildReqBody (Chain _ prompt schema) maybeFormat =
  GenerateContentRequest contents (Just $ mapCommonSchemaDefinition $ convertJson schema)
  where
    contents = [Content (Role role) [Part $ unviewable content] | ReqMessage role content <- maybe prompt (`formatPrompt` prompt) maybeFormat]
buildReqBody (StrChain _ prompt) maybeFormat =
  GenerateContentRequest contents Nothing
  where
    contents = [Content (Role role) [Part $ unviewable content] | ReqMessage role content <- maybe prompt (`formatPrompt` prompt) maybeFormat]

buildOutput :: Chain b a -> GenerateContentResponse -> Either LgchainError (Output a)
buildOutput (Chain {}) res = case extractStructedOutput res of
  Just output -> Right output
  Nothing -> Left $ ParsingError "Failed to parse structured output from response"
buildOutput (StrChain _ _) res = case extractStrOutput res of
  Just str -> Right $ StrOutput str
  Nothing -> Left $ ParsingError "Failed to extract string output from response"

invokeGemini :: Chain ChatGemini a -> Maybe FormatMap -> ExceptT LgchainError IO (Output a)
invokeGemini chain formatMap = do
  geminiApiKey <- liftIO $ getEnv "GEMINI_API_KEY"
  let model = case chain of
        Chain m _ _ -> m
        StrChain m _ -> m
  let modelName = geminiModelNameStr model
  let formattedReq = buildReqBody chain formatMap
  let req =
        setRequestHeaders [(hContentType, "application/json")] $
          setRequestBodyJSON formattedReq $
            setRequestQueryString [("key", Just $ BS.pack $ UTF8.encode geminiApiKey)] $
              parseRequest_ $
                "POST https://generativelanguage.googleapis.com/v1beta/models/" ++ modelName ++ ":generateContent"
  res <- liftIO $ do
    response <- httpJSON req
    return $ getResponseBody response
  case buildOutput chain res of
    Right output -> return output
    Left err -> throwE err

instance LLMModel ChatGemini where
  invokeWithSchema model prompt schema = invokeGemini (Chain model prompt schema)
  invokeStr model prompt = invokeGemini (StrChain model prompt)
