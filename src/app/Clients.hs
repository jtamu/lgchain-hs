module Clients where

import Data.ByteString.Char8 qualified as BS
import Data.Map.Strict qualified as M
import Data.Text qualified as T
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import Requests (Prompt, ReqBody (ReqBody), ReqMessage (ReqMessage), ResponseFormat)
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

formatPrompt :: Prompt -> FormatMap -> Prompt
formatPrompt prompt formatMap = map (\(ReqMessage role content) -> ReqMessage role (formatAll content formatMap)) prompt

openAIModelNameStr :: ChatOpenAI -> String
openAIModelNameStr (ChatOpenAI modelName) = show modelName

strOutput :: ResBody -> Maybe String
strOutput resBody = case choices resBody of
  (ResMessage message : _) -> Just $ content message
  _ -> Nothing

buildReqBody :: ChatOpenAI -> Prompt -> Maybe FormatMap -> Maybe ResponseFormat -> ReqBody
buildReqBody model prompt (Just formatMap) = ReqBody (openAIModelNameStr model) (formatPrompt prompt formatMap)
buildReqBody model prompt Nothing = ReqBody (openAIModelNameStr model) prompt

invoke :: ChatOpenAI -> Prompt -> Maybe FormatMap -> Maybe ResponseFormat -> IO ResBody
invoke model prompt formatMap resFormat = do
  openaiApiKey <- getEnv "OPENAI_API_KEY"
  let reqbody = buildReqBody model prompt formatMap resFormat
  let req = setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $ setRequestBodyJSON reqbody $ parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- httpJSON req
  return (getResponseBody res :: ResBody)
