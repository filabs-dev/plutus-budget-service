{- HLINT ignore "Redundant <$>" -}
{-# Language OverloadedStrings #-}

{-|
Module      : Utils
Description : Utils functions for the server.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define common functions used by the evaluate server.
This functions help to build the responses of the server and logging
the information of each evaluation
-}

module Utils where

import           Data.Maybe                  ( fromMaybe, fromJust, catMaybes )
import qualified Data.Aeson            as A
import qualified Data.HashMap.Internal as HM ( lookup )
import qualified Data.ByteString.Lazy  as BL ( ByteString )
import qualified Data.Text             as T  ( pack, unpack )
import           Data.Time.Clock             ( getCurrentTime )

import Network.Wai ( Request
                   , Response
                   , requestMethod
                   , rawPathInfo
                   , responseLBS
                   , responseBuilder
                   )
import Network.Wai.Handler.Warp ( Port )

import Network.HTTP.Types ( status200, status405
                          , hContentType
                          )

import System.Environment ( lookupEnv, getArgs )

import Plutus.Contract.Wallet ( ExportTx(..) )

import Evaluate ( evaluate )
import Config ( Config )

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

logExportTx :: Either String A.Value -> IO ()
logExportTx (Left err) = putStrLn $
                                unwords ["Error decoding body request:", err]
logExportTx (Right json) =
    case json of
        A.Object obj -> do
            let (A.String tx)  = fromJust $ HM.lookup "transaction" obj
            putStrLn $
                unwords [ "Transaction in Body Request:"
                        , take 40 (T.unpack tx) ++ "..."
                        ]
        _ -> putStrLn "Expected object type"

logRequest :: Request -> IO ()
logRequest req = do
    time <- getCurrentTime
    putStrLn $ show time
    putStrLn $ unwords ["Request method:", show $ requestMethod req ]
    putStrLn $ unwords ["Request path:", show $ rawPathInfo req ]
