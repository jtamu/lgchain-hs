{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Data.Aeson (decode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Text qualified as T
import Lgchain.Core.Requests (FormatMap, Prompt, ReqMessage (ReqMessage), formatPrompt)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (hContentType)
import Requests (Content (Content), GenerateContentRequest (GenerateContentRequest), JsonSchemaConvertable (convertJson), Part (Part), Role (Role))
import Responses (Content (parts), GenerateContentResponse (GenerateContentResponse), Part (text))
import Responses qualified as Res (Candidate (content))
import System.Environment (getEnv)

data GeminiModelName = GEMINI_1_5_FLASH

instance Show GeminiModelName where
  show GEMINI_1_5_FLASH = "gemini-1.5-flash"

newtype ChatGemini = ChatGemini {modelName :: GeminiModelName} deriving (Show)

geminiModelNameStr :: ChatGemini -> String
geminiModelNameStr (ChatGemini modelName) = show modelName

data Output a where
  StrOutput :: String -> Output a
  StructedOutput :: (JsonSchemaConvertable a) => a -> Output a

deriving instance Show (Output a)

deriving instance Eq (Output a)

extractStrOutput :: GenerateContentResponse -> Maybe String
extractStrOutput (GenerateContentResponse candidates) =
  let messages = [text part | candidate <- candidates, part <- parts $ Res.content candidate]
   in case messages of
        (message : _) -> Just $ T.unpack message
        _ -> Nothing

strOutput :: Maybe (Output a) -> Maybe String
strOutput (Just (StrOutput str)) = Just str
strOutput _ = Nothing

extractStructedOutput :: (JsonSchemaConvertable a) => GenerateContentResponse -> Maybe (Output a)
extractStructedOutput res = do
  str <- extractStrOutput res
  decoded <- decode $ LBS.pack $ UTF8.encode str
  return $ StructedOutput decoded

structedOputput :: (JsonSchemaConvertable a) => Maybe (Output a) -> Maybe a
structedOputput (Just (StructedOutput a)) = Just a
structedOputput _ = Nothing

data Chain a where
  Chain :: (JsonSchemaConvertable a) => ChatGemini -> Prompt -> a -> Chain a
  StrChain :: ChatGemini -> Prompt -> Chain a

buildReqBody :: Chain a -> Maybe FormatMap -> GenerateContentRequest
buildReqBody (Chain _ prompt schema) maybeFormat =
  GenerateContentRequest contents (Just $ convertJson schema)
  where
    contents = [Content (Role role) [Part content] | ReqMessage role content <- maybe prompt (`formatPrompt` prompt) maybeFormat]
buildReqBody (StrChain _ prompt) maybeFormat =
  GenerateContentRequest contents Nothing
  where
    contents = [Content (Role role) [Part content] | ReqMessage role content <- maybe prompt (`formatPrompt` prompt) maybeFormat]

buildOutput :: Chain a -> GenerateContentResponse -> Maybe (Output a)
buildOutput (Chain {}) res = extractStructedOutput res
buildOutput (StrChain _ _) res = StrOutput <$> extractStrOutput res

invoke :: Chain a -> Maybe FormatMap -> IO (Maybe (Output a))
invoke chain formatMap = do
  geminiApiKey <- getEnv "GEMINI_API_KEY"
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
  res <- httpJSON req
  let resBody = getResponseBody res
  return $ buildOutput chain resBody
