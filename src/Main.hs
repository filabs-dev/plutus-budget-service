{- HLINT ignore "Redundant <$>" -}
{-# Language OverloadedStrings #-}

module Main (main) where

import           Data.Maybe                 ( fromMaybe, catMaybes )
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

import System.Environment ( lookupEnv, getArgs )

import Plutus.Contract.Wallet ( ExportTx(..) )

import Estimate ( estimate, Config )

main :: IO ()
main = do
    port <- getPort
    config <- getConfig
    putStrLn $ unwords ["Starting estimate-server at port:", show port]
    putStrLn "Quit the server with CONTROL-C."
    run port $ corsWithContentType $ estimateApp config
  where
    corsWithContentType :: Middleware
    corsWithContentType =
      let policy = simpleCorsResourcePolicy
                    { corsRequestHeaders = ["Content-Type"] }
      in cors (const $ Just policy)

webAppEstimate :: Config -> Request -> IO Response
webAppEstimate conf req =
    case requestMethod req of
        "POST" -> (BL.toStrict <$> consumeRequestBodyStrict req)
                  >>= processEstimateMessage conf . A.eitherDecodeStrict
                  >>= generateResponse . A.encode
        _otherMethod -> pure badRequest

estimateApp :: Config -> Application
estimateApp conf req send =
    case pathInfo req of
        ["estimate"] -> webAppEstimate conf req
        _            -> pure badRequest
    >>= send

generateResponse :: BL.ByteString -> IO Response
generateResponse = pure .
                   responseLBS status200
                   [(hContentType, "application/json")]

processEstimateMessage :: Config -> Either String A.Value -> IO A.Value
processEstimateMessage _ (Left err)   = return (A.toJSON err)
processEstimateMessage conf (Right json) =
    case A.fromJSON json :: A.Result ExportTx of
        A.Success etx -> return $ A.toJSON $ estimate conf etx
        A.Error err   -> return $ A.String $ T.pack err

getPort :: IO Port
getPort = do
    portEnv <- lookupEnv "PORT"
    portArg <- findNext (=="--port") <$> getArgs
    let port = head $ catMaybes [portArg, portEnv, Just "3001"]
    return $ read port

getConfig :: IO Config
getConfig = do
    confFile <- findNext (=="--config") <$> getArgs
    let fileCont = fromMaybe
                        (error "Must provide a config file under the --config flag")
                        confFile
    mConf <- A.eitherDecodeFileStrict fileCont :: IO (Either String Config)
    let conf = either error id mConf

    return conf

badRequest :: Response
badRequest = responseBuilder status405 [] "Bad request method"

findNext :: (a -> Bool) -> [a] -> Maybe a
findNext _ [] = Nothing
findNext _ [_] = Nothing
findNext p (x:y:xs) | p x = Just y
                    | otherwise = findNext p (y:xs)
