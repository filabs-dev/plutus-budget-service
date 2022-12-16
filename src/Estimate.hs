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

module Estimate where

import           Data.Array                  (Array)
import           Data.Aeson                  hiding (Array)
import           Data.Aeson.Types            hiding (Array)
import qualified Data.ByteString.Char8 as B8
import           Data.Functor.Identity       ( runIdentity )
import qualified Data.Map              as M  (Map, fromList)
import           Data.Maybe.Strict           (StrictMaybe, maybeToStrictMaybe)
import           Data.Quantity               (Quantity (..))
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime, POSIXTime)
import qualified Data.Text             as T

import           Control.Arrow              ( left )
import           Control.Monad.Trans.Except ( runExceptT )

import           GHC.Records ( HasField (..) )

import           Plutus.Contract.Wallet ( ExportTx(..), ExportTxInput (..) )

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Ledger.Alonzo.Scripts   ( ExUnits )
import           Cardano.Ledger.Alonzo.TxWitness ( RdmrPtr (..) )
import           Cardano.Ledger.Alonzo.Tools ( BasicFailure
                                             , ScriptFailure
                                             , evaluateTransactionExecutionUnits
                                             )
import qualified Cardano.Ledger.Alonzo          as Alonzo
import qualified Cardano.Ledger.Alonzo.TxBody   as Alonzo
import qualified Cardano.Ledger.Alonzo.PParams  as Alonzo
import qualified Cardano.Ledger.Alonzo.Scripts  as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo

import qualified Cardano.Ledger.Core     as Core   ( Tx, PParams )
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
                                                , ProtocolParameters(..)
                                                , toMaryValue
                                                , toShelleyAddr
                                                , toShelleyTxIn
                                                )

import Cardano.Wallet.Transaction ( ErrAssignRedeemers (..) )
import Cardano.Wallet.Shelley.Compatibility ( toAlonzoPParams
                                            , toCostModelsAsArray
                                            )
import Cardano.Wallet.Primitive.Types.Hash ( Hash(..) )

import Cardano.Wallet.Primitive.Slotting ( PastHorizonException
                                         , TimeInterpreter
                                         , TimeInterpreter
                                         , getSystemStart
                                         , toEpochInfo
                                         , hoistTimeInterpreter
                                         , mkSingleEraInterpreter
                                         )

import Cardano.Wallet.Primitive.Types ( ActiveSlotCoefficient (..)
                                      , EpochLength (..)
                                      , GenesisParameters (..)
                                      , SlotLength (..)
                                      , SlottingParameters (..)
                                      , StartTime (..)
                                      )

import Cardano.Slotting.Time          ( SystemStart )
import Cardano.Slotting.EpochInfo.API ( hoistEpochInfo, EpochInfo )
import GHC.Generics (Generic)

data Config = Config {
    protocolParameters :: Shelley.ProtocolParameters,
    genesisTime        :: POSIXTime
} deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data EstimateError = ErrAssign ErrAssignRedeemers
                   | BFailure  (BasicFailure StandardCrypto)
    deriving Show

type AlonzoTx = Core.Tx (Shelley.ShelleyLedgerEra Shelley.AlonzoEra)
type RedeemerReport = M.Map RdmrPtr
                            (Either (ScriptFailure StandardCrypto) ExUnits)

instance ToJSON EstimateError where
    toJSON (ErrAssign errAssign) = object ["ErrAssign" .= show errAssign]
    toJSON (BFailure bFail)      = object ["BFailure" .= show bFail]

instance ToJSON (ScriptFailure StandardCrypto) where
    toJSON a = object ["ScriptFail" .= show a]

instance ToJSON RdmrPtr where
    toJSON (RdmrPtr tag w) = object ["tag" .= show tag, "index" .= show w]

instance ToJSONKey RdmrPtr where
    toJSONKey = toJSONKeyText
                (\(RdmrPtr tag w) -> T.pack $ show tag ++ ":" ++ show w)

instance {-# OVERLAPPING #-}
    ToJSON (Either (ScriptFailure StandardCrypto) ExUnits) where
    toJSON (Left err) = toJSON err
    toJSON (Right eu) = toJSON eu

estimate
    :: Config
    -> ExportTx
    -> Either EstimateError RedeemerReport
estimate conf ExportTx{..} =
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

ti :: Config -> TimeInterpreter (Either PastHorizonException)
ti = dummyTimeInterpreter

systemStart :: Config -> SystemStart
systemStart = getSystemStart . ti

mkEpochInfo :: Config -> Either EstimateError (EpochInfo (Either EstimateError))
mkEpochInfo conf =
    hoistEpochInfo
    (left (ErrAssign . ErrAssignRedeemersPastHorizon) . runIdentity . runExceptT)
    <$>
    left (ErrAssign . ErrAssignRedeemersPastHorizon) (toEpochInfo . ti $ conf)

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

-- CODE STOLEN FROM:
-- https://github.com/input-output-hk/cardano-wallet/blob/06a5b176123c6b540ed1bc8a783953b07bba0a66/lib/core/test-common/Cardano/Wallet/DummyTarget/Primitive/Types.hs

dummyTimeInterpreter :: Monad m => Config -> TimeInterpreter m
dummyTimeInterpreter conf = hoistTimeInterpreter (pure . runIdentity) $
                            mkSingleEraInterpreter
                            (getGenesisBlockDate $ dummyGenesisParameters conf)
                            dummySlottingParameters

-- Preprod empirical start time. It follows what's written in nft-vault PAB config comments:
-- zero time taken from blockfrost API and cardanoscan is 1654041600 in secs
-- https://docs.blockfrost.io/#tag/Cardano-Ledger/paths/~1genesis/get
-- which gaves us 1654041600000 in miliseconds as the official start time.
-- 1654041600000 = GMT: Wednesday, June 1, 2022 12:00:00 AM
-- This number doesn't work: it shifts our validation interval too much into the future.
-- Empirically using Ogmios and BuiltinShow for printing on-chain comparison
-- we got 1655683200000 = GMT: Monday, June 20, 2022 12:00:00 AM
-- 19 days after the "official" time.
-- In seconds, it is 1655683200
dummyGenesisParameters :: Config -> GenesisParameters
dummyGenesisParameters conf =
    GenesisParameters
    { getGenesisBlockHash = dummyGenesisHash
    , getGenesisBlockDate = StartTime . posixSecondsToUTCTime $ genesisTime conf
    }

dummyGenesisHash :: Hash "Genesis"
dummyGenesisHash = Hash (B8.replicate 32 '1')

dummySlottingParameters :: SlottingParameters
dummySlottingParameters =
    SlottingParameters
    { getSlotLength = SlotLength 1
    , getEpochLength = EpochLength 432000
    , getActiveSlotCoefficient = ActiveSlotCoefficient 1
    , getSecurityParameter = Quantity 2160
    }
