{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DerivingStrategies   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE DeriveAnyClass       #-}

{-|
Module      : Evaluate
Description : Main logic for evaluating ExportTxs.
Copyright   : (c) 2022 IDYIA LLC dba Plank
Maintainer  : opensource@joinplank.com
Stability   : develop

We define the functions to transfrom the ExporTx to the required types
for the evaluateTransactionExecutionUnits defined in the ledger.
-}

module Evaluate where

import           Data.Array                  ( Array )
import qualified Data.Map              as M  ( fromList )
import           Data.Maybe.Strict           ( StrictMaybe, maybeToStrictMaybe )
import           GHC.Records                 ( HasField (..) )
import           Plutus.Contract.Wallet      ( ExportTx (..), ExportTxInput (..) )
import qualified Cardano.Crypto.Hash.Class as Crypto
import           Cardano.Ledger.Alonzo.Tools     ( evaluateTransactionExecutionUnits )
import qualified Cardano.Ledger.Alonzo          as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody   as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams  as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts  as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo

import qualified Cardano.Ledger.Core     as Core   ( PParams )
import           Cardano.Ledger.Crypto             ( StandardCrypto )
import qualified Cardano.Ledger.Hashes   as Hashes ( EraIndependentData )
import           Cardano.Ledger.SafeHash           ( SafeHash
                                                   , unsafeMakeSafeHash
                                                   , castSafeHash
                                                   )
import qualified Cardano.Ledger.Shelley.UTxO as UTxO ( UTxO(..) )
import qualified Cardano.Ledger.TxIn         as LedgerTxIn ( TxIn )

import qualified Cardano.Api         as Api     ( AssetId(..)
                                                , Value
                                                , lovelaceToQuantity
                                                , serialiseToRawBytes
                                                , valueFromList
                                                )
import qualified Cardano.Api.Shelley as Shelley ( AlonzoEra, TxIn(..)
                                                , Tx(ShelleyTx)
                                                , ShelleyLedgerEra
                                                , toMaryValue
                                                , toShelleyAddr
                                                , toShelleyTxIn
                                                )

import Cardano.Wallet.Shelley.Compatibility ( toAlonzoPParams
                                            , toCostModelsAsArray
                                            )

import Config ( mkEpochInfo
              , Config (protocolParameters)
              , systemStart
              , EvaluateError (..)
              , RedeemerReport
              , AlonzoTx
              )

evaluate
    :: Config
    -> ExportTx
    -> Either EvaluateError RedeemerReport
evaluate conf ExportTx{..} =
    mkEpochInfo conf >>= \epochInfo ->
    case
        evaluateTransactionExecutionUnits
        pparams
        alonzoTx
        utxos
        epochInfo
        (systemStart conf)
        costs
    of
        Right (Right a) -> Right a
        Right (Left err) -> Left $ BFailure err
        Left err -> Left err
  where
    alonzoTx :: AlonzoTx
    (Shelley.ShelleyTx _ alonzoTx) = partialTx

    pparams :: Core.PParams (Alonzo.AlonzoEra StandardCrypto)
    pparams = toAlonzoPParams $ protocolParameters conf

    utxos :: UTxO.UTxO (Shelley.ShelleyLedgerEra Shelley.AlonzoEra)
    utxos = utxosFromExportTxInputs lookups

    costs :: Array Alonzo.Language Alonzo.CostModel
    costs = toCostModelsAsArray (getField @"_costmdls" pparams)

utxosFromExportTxInputs
    :: [ExportTxInput]
    -> UTxO.UTxO (Shelley.ShelleyLedgerEra Shelley.AlonzoEra)
utxosFromExportTxInputs insInfo =
    UTxO.UTxO $
    M.fromList [ utxoFromInputInfo input | input <- insInfo ]

utxoFromInputInfo
    :: ExportTxInput
    -> ( LedgerTxIn.TxIn StandardCrypto
       , Alonzo.TxOut (Alonzo.AlonzoEra StandardCrypto)
       )
utxoFromInputInfo ExportTxInput{..} = (utxoTxIn, utxoTxOut)
  where
    utxoTxIn :: LedgerTxIn.TxIn StandardCrypto
    utxoTxIn = Shelley.toShelleyTxIn $ Shelley.TxIn etxiId etxiTxIx

    utxoTxOut :: Alonzo.TxOut (Alonzo.AlonzoEra StandardCrypto)
    utxoTxOut = Alonzo.TxOut
                (Shelley.toShelleyAddr etxiAddress)
                (Shelley.toMaryValue val)
                mDatumHash

    val :: Api.Value
    val = Api.valueFromList $
          (Api.AdaAssetId, Api.lovelaceToQuantity etxiLovelaceQuantity) :
          [(Api.AssetId id' an, q) | (id', an, q) <- etxiAssets]

    mDatumHash :: StrictMaybe
                 (SafeHash StandardCrypto Hashes.EraIndependentData)
    mDatumHash =
        maybeToStrictMaybe $
        castSafeHash . unsafeMakeSafeHash <$>
        (etxiDatumHash >>= Crypto.hashFromBytes . Api.serialiseToRawBytes)
