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

import Evaluate ( evaluate, Config )

main :: IO ()
main = do
    port <- getPort
    config <- getConfig
    putStrLn $ unwords ["Starting budget-service at port:", show port]
    putStrLn "Quit the service with CONTROL-C."
    run port $ corsWithContentType $ evaluationApp config
  where
    corsWithContentType :: Middleware
    corsWithContentType =
      let policy = simpleCorsResourcePolicy
                    { corsRequestHeaders = ["Content-Type"] }
      in cors (const $ Just policy)

webAppEvaluate :: Config -> Request -> IO Response
webAppEvaluate conf req =
    case requestMethod req of
        "POST" -> (BL.toStrict <$> consumeRequestBodyStrict req)
                  >>= processEvaluateMessage conf . A.eitherDecodeStrict
                  >>= generateResponse . A.encode
        _otherMethod -> pure badRequest

evaluationApp :: Config -> Application
evaluationApp conf req send =
    case pathInfo req of
        ["evaluate"] -> webAppEvaluate conf req
        ["estimate"] -> webAppEvaluate conf req -- For legacy support.
        _            -> pure badRequest
    >>= send

generateResponse :: BL.ByteString -> IO Response
generateResponse = pure .
                   responseLBS status200
                   [(hContentType, "application/json")]

processEvaluateMessage :: Config -> Either String A.Value -> IO A.Value
processEvaluateMessage _ (Left err)   = return (A.toJSON err)
processEvaluateMessage conf (Right json) =
    case A.fromJSON json :: A.Result ExportTx of
        A.Success etx -> return $ A.toJSON $ evaluate conf etx
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
                        (error "Must provide a config file under the flag --config [path to file].")
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
