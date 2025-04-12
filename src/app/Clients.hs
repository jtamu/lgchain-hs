module Clients where

import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import Requests (Prompt, ReqBody (ReqBody))
import Responses (ResBody (choices), ResMessage (ResMessage), ResMessageContent (content))
import System.Environment (getEnv)

newtype ChatOpenAI = ChatOpenAI {model_name :: String} deriving (Show)

strOutput :: ResBody -> Maybe String
strOutput resBody = case choices resBody of
  (ResMessage message : _) -> Just $ content message
  _ -> Nothing

invoke :: ChatOpenAI -> Prompt -> IO ResBody
invoke model prompt = do
  openaiApiKey <- getEnv "OPENAI_API_KEY"
  let reqbody = ReqBody (model_name model) prompt
  let req = setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $ setRequestBodyJSON reqbody $ parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- httpJSON req
  return (getResponseBody res :: ResBody)
