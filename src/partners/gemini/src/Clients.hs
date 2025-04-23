{-# LANGUAGE OverloadedStrings #-}

module Clients where

import Codec.Binary.UTF8.String qualified as UTF8
import Data.ByteString qualified as BS
import Data.Map qualified as M
import Data.Text qualified as T
import Network.HTTP.Simple (getResponseBody, httpJSON, parseRequest_, setRequestBodyJSON, setRequestHeaders, setRequestQueryString)
import Network.HTTP.Types (hContentType)
import Requests (Content (Content), GenerateContentRequest (GenerateContentRequest), Part (Part))
import Responses (GenerateContentResponse)
import System.Environment (getEnv)

data GeminiModelName = GEMINI_1_5_FLASH

instance Show GeminiModelName where
  show GEMINI_1_5_FLASH = "gemini-1.5-flash"

newtype ChatGemini = ChatGemini {modelName :: GeminiModelName} deriving (Show)

type FormatMap = M.Map T.Text T.Text

formatAll :: T.Text -> FormatMap -> T.Text
formatAll = M.foldlWithKey (\acc k v -> T.replace k v acc)

formatPrompt :: FormatMap -> GenerateContentRequest -> GenerateContentRequest
formatPrompt formatMap (GenerateContentRequest contents config) =
  GenerateContentRequest
    [ Content role (map (\(Part part) -> Part (formatAll part formatMap)) parts) | Content role parts <- contents
    ]
    config

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
  StrChain :: ChatGemini -> GenerateContentRequest -> Chain a

invoke :: Chain a -> Maybe FormatMap -> IO GenerateContentResponse
invoke (StrChain model reqBody) formatMap = do
  geminiApiKey <- getEnv "GEMINI_API_KEY"
  let modelName = geminiModelNameStr model
  let formattedReq = maybe reqBody (`formatPrompt` reqBody) formatMap
  let req =
        setRequestHeaders [(hContentType, "application/json")] $
          setRequestBodyJSON formattedReq $
            setRequestQueryString [("key", Just $ BS.pack $ UTF8.encode geminiApiKey)] $
              parseRequest_ $
                "POST https://generativelanguage.googleapis.com/v1beta/models/" ++ modelName ++ ":generateContent"
  res <- httpJSON req
  return $ getResponseBody res
