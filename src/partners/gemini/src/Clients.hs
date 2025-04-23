{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Data.ByteString qualified as BS
import Data.Map qualified as M
import Data.Text qualified as T
import GHC.Generics (Generic)
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (hContentType)
import Requests (Content (Content), GenerateContentRequest (GenerateContentRequest), Part (Part), Role)
import Responses (GenerateContentResponse)
import System.Environment (getEnv)

data GeminiModelName = GEMINI_1_5_FLASH

instance Show GeminiModelName where
  show GEMINI_1_5_FLASH = "gemini-1.5-flash"

newtype ChatGemini = ChatGemini {modelName :: GeminiModelName} deriving (Show)

type FormatMap = M.Map T.Text T.Text

formatAll :: T.Text -> FormatMap -> T.Text
formatAll = M.foldlWithKey (\acc k v -> T.replace k v acc)

data ReqMessage = ReqMessage {role :: Role, content :: T.Text} deriving (Eq, Show, Generic)

type Prompt = [ReqMessage]

formatPrompt :: FormatMap -> Prompt -> Prompt
formatPrompt formatMap prompt = [ReqMessage role (formatAll content formatMap) | ReqMessage role content <- prompt]

geminiModelNameStr :: ChatGemini -> String
geminiModelNameStr (ChatGemini modelName) = show modelName

data Output a where
  StrOutput :: String -> Output a

deriving instance Show (Output a)

deriving instance Eq (Output a)

strOutput :: Maybe (Output a) -> Maybe String
strOutput (Just (StrOutput str)) = Just str
strOutput _ = Nothing

data Chain a where
  StrChain :: ChatGemini -> Prompt -> Chain a

buildReqBody :: Chain a -> Maybe FormatMap -> GenerateContentRequest
buildReqBody (StrChain _ prompt) maybeFormat =
  GenerateContentRequest contents Nothing
  where
    contents = [Content role [Part content] | ReqMessage role content <- maybe prompt (`formatPrompt` prompt) maybeFormat]

invoke :: Chain a -> Maybe FormatMap -> IO GenerateContentResponse
invoke chain@(StrChain model _) formatMap = do
  geminiApiKey <- getEnv "GEMINI_API_KEY"
  let modelName = geminiModelNameStr model
  let formattedReq = buildReqBody chain formatMap
  let req =
        setRequestHeaders [(hContentType, "application/json")] $
          setRequestBodyJSON formattedReq $
            setRequestQueryString [("key", Just $ BS.pack $ UTF8.encode geminiApiKey)] $
              parseRequest_ $
                "POST https://generativelanguage.googleapis.com/v1beta/models/" ++ modelName ++ ":generateContent"
  res <- httpJSON req
  return $ getResponseBody res
