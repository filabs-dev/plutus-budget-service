{- HLINT ignore "Redundant <$>" -}
{-# Language OverloadedStrings #-}

{-|
Module      : Main
Description : Main functions for the server.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

Functions for the server and its configurations.
-}

module Main (main) where

import qualified Data.Aeson            as A
import qualified Data.ByteString.Lazy  as BL ( toStrict )

import Network.Wai ( Application
                   , Request
                   , Response
                   , Middleware
                   , pathInfo
                   , consumeRequestBodyStrict
                   , requestMethod
                   , responseStatus
                   )
import Network.Wai.Handler.Warp ( run )

import Network.Wai.Middleware.Cors ( cors
                                   , simpleCorsResourcePolicy
                                   , corsRequestHeaders
                                   )

import Config ( Config )
import Utils  ( generateResponse
              , processEvaluateMessage
              , getPort
              , getConfig
              , badRequest
              , logExportTx
              , logRequest
              )

-- | Runs the evaluation server
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

{- | Performs the evaluation of the given Request
    according of the configuration used.
-}
webAppEvaluate :: Config -> Request -> IO Response
webAppEvaluate conf req = do
    logRequest req
    case requestMethod req of
        "POST" -> do
            body <- (BL.toStrict <$> consumeRequestBodyStrict req)
            let exportTx = A.eitherDecodeStrict body
            evalMessage <- processEvaluateMessage conf exportTx
            logExportTx exportTx
            generateResponse $ A.encode evalMessage
        _otherMethod -> do
            putStrLn $ unwords [ "Bad Request Error status:"
                               , show $ responseStatus badRequest
                               ]
            pure badRequest

{- | Given a Config it performs the application of the evaluate,
    if the endpoint used by the request is "evaluate".
-}
evaluationApp :: Config -> Application
evaluationApp conf req send =
    case pathInfo req of
        ["evaluate"] -> webAppEvaluate conf req
        ["estimate"] -> webAppEvaluate conf req -- For legacy support.
        _            -> pure badRequest
    >>= send
