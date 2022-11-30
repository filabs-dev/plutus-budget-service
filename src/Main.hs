{- HLINT ignore "Redundant <$>" -}
{-# Language OverloadedStrings #-}

module Main (main) where

import           Data.Maybe                 ( fromMaybe )
import qualified Data.Aeson           as A
import qualified Data.ByteString.Lazy as BL ( ByteString, toStrict )
import qualified Data.Text            as T  ( pack )

import Network.Wai ( Application
                   , Request
                   , Response
                   , Middleware
                   , pathInfo
                   , responseLBS
                   , consumeRequestBodyStrict
                   , requestMethod
                   , responseBuilder
                   )
import Network.Wai.Handler.Warp (Port, run)

import Network.Wai.Middleware.Cors

import Network.HTTP.Types ( status200, status405
                          , hContentType
                          )

import System.Environment ( lookupEnv )

import Plutus.Contract.Wallet ( ExportTx(..) )

import Estimate ( estimate )

main :: IO ()
main = do
  port <- getPort
  putStrLn $ unwords ["Starting estimate-server at port:", show port]
  putStrLn "Quit the server with CONTROL-C."
  run port $ corsWithContentType estimateApp
 where
  corsWithContentType :: Middleware
  corsWithContentType =
    let policy = simpleCorsResourcePolicy
                   { corsRequestHeaders = ["Content-Type"] }
    in cors (const $ Just policy)

webAppEstimate :: Request -> IO Response
webAppEstimate req =
    case requestMethod req of
        "POST" -> (BL.toStrict <$> consumeRequestBodyStrict req)
                  >>= processEstimateMessage . A.eitherDecodeStrict
                  >>= generateResponse . A.encode
        _otherMethod -> pure badRequest

estimateApp :: Application
estimateApp req send =
    case pathInfo req of
        ["estimate"] -> webAppEstimate req
        _            -> pure badRequest
    >>= send

generateResponse :: BL.ByteString -> IO Response
generateResponse = pure .
                   responseLBS status200
                   [(hContentType, "application/json")]

processEstimateMessage :: Either String A.Value -> IO A.Value
processEstimateMessage (Left err)   = return (A.toJSON err)
processEstimateMessage (Right json) =
    case A.fromJSON json :: A.Result ExportTx of
        A.Success etx -> return $ A.toJSON $ estimate etx
        A.Error err   -> return $ A.String $ T.pack err

getPort :: IO Port
getPort = do
  portEnv <- lookupEnv "PORT"
  let port = fromMaybe "3001" portEnv
  return $ read port

badRequest :: Response
badRequest = responseBuilder status405 [] "Bad request method"
