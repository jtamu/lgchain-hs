module Main where

import Data.ByteString.Char8 qualified as BS
import Network.HTTP.Conduit (parseRequest_)
import Network.HTTP.Simple (getResponseBody, httpJSON, setRequestBodyJSON, setRequestHeaders)
import Network.HTTP.Types (hAuthorization, hContentType)
import Requests (ReqBody (ReqBody), ReqMessage (ReqMessage))
import Responses (ResBody (choices), ResMessage (ResMessage), ResMessageContent (content))

openaiApiKey :: String
openaiApiKey = "sk-..."

main :: IO ()
main = do
  let reqbody = ReqBody "gpt-4o" [ReqMessage "system" "語尾にニャーとつけてください", ReqMessage "user" "猫は好きですか？"]
  let req = setRequestHeaders [(hAuthorization, BS.pack $ "Bearer " ++ openaiApiKey), (hContentType, BS.pack "application/json")] $ setRequestBodyJSON reqbody $ parseRequest_ "POST https://api.openai.com/v1/chat/completions"
  res <- httpJSON req
  let resBody = getResponseBody res :: ResBody

  case choices resBody of
    (ResMessage message : _) -> putStrLn $ content message
    _ -> putStrLn "No response received"
