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

module Estimate where

import           Data.Array                  (Array)
import           Data.Aeson                  hiding (Array)
import           Data.Aeson.Types            hiding (Array)
import qualified Data.ByteString.Char8 as B8
import           Data.Functor.Identity       ( runIdentity )
import qualified Data.Map              as M  (Map, fromList)
import           Data.Maybe.Strict           (StrictMaybe, maybeToStrictMaybe)
import           Data.Quantity               (Quantity (..))
import           Data.Ratio                  ((%))
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime)
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

import qualified Cardano.Api         as Api     ( AnyPlutusScriptVersion(..)
                                                , AssetId(..)
                                                , CostModel(..)
                                                , EpochNo(..)
                                                , ExecutionUnits(..)
                                                , ExecutionUnitPrices(..)
                                                , PlutusScriptVersion(..)
                                                , Lovelace(..)
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
    :: ExportTx
    -> Either EstimateError RedeemerReport
estimate ExportTx{..} =
    mkEpochInfo >>= \epochInfo ->
    case
        evaluateTransactionExecutionUnits
        pparams
        alonzoTx
        utxos
        epochInfo
        systemStart
        costs
    of
        Right (Right a) -> Right a
        Right (Left err) -> Left $ BFailure err
        Left err -> Left err
  where
    alonzoTx :: AlonzoTx
    (Shelley.ShelleyTx _ alonzoTx) = partialTx

    pparams :: Core.PParams (Alonzo.AlonzoEra StandardCrypto)
    pparams = toAlonzoPParams pp

    utxos :: UTxO.UTxO (Shelley.ShelleyLedgerEra Shelley.AlonzoEra)
    utxos = utxosFromExportTxInputs lookups

    costs :: Array Alonzo.Language Alonzo.CostModel
    costs = toCostModelsAsArray (getField @"_costmdls" pparams)

ti :: TimeInterpreter (Either PastHorizonException)
ti = dummyTimeInterpreter

systemStart :: SystemStart
systemStart = getSystemStart ti

mkEpochInfo :: Either EstimateError (EpochInfo (Either EstimateError))
mkEpochInfo =
    hoistEpochInfo
    (left (ErrAssign . ErrAssignRedeemersPastHorizon) . runIdentity . runExceptT)
    <$>
    left (ErrAssign . ErrAssignRedeemersPastHorizon) (toEpochInfo ti)

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

-- Parameters obtained directly from the cardano-node of the private testnet.
pp :: Shelley.ProtocolParameters
pp = Shelley.ProtocolParameters
    { protocolParamProtocolVersion = (7,0)
    , protocolParamDecentralization = 7 % 10
    , protocolParamExtraPraosEntropy = Nothing
    , protocolParamMaxBlockHeaderSize = 1100
    , protocolParamMaxBlockBodySize = 98304
    , protocolParamMaxTxSize = 16384
    , protocolParamTxFeeFixed = 155381
    , protocolParamTxFeePerByte = 44
    , protocolParamMinUTxOValue = Nothing
    , protocolParamStakeAddressDeposit = Api.Lovelace 2000000
    , protocolParamStakePoolDeposit = Api.Lovelace 500000000
    , protocolParamMinPoolCost = Api.Lovelace 340000000
    , protocolParamPoolRetireMaxEpoch = Api.EpochNo 18
    , protocolParamStakePoolTargetNum = 500
    , protocolParamPoolPledgeInfluence = 3 % 10
    , protocolParamMonetaryExpansion = 3 % 1000
    , protocolParamTreasuryCut = 1 % 5
    , protocolParamUTxOCostPerWord = Just (Api.Lovelace 4310)
    , protocolParamCostModels =
      M.fromList [
            ( Api.AnyPlutusScriptVersion Api.PlutusScriptV1
            , Api.CostModel
                (M.fromList
                -- https://github.com/input-output-hk/cardano-node/blob/d0b0fa10ac83dc1d0d40aaf294cfded1b455a0a4/scripts/babbage/alonzo-babbage-test-genesis.json
                -- it is a merge between V1 and V2 cost models: names of V1 and V2 values.
                    [  ("addInteger-cpu-arguments-intercept", 205665)
                    ,  ("addInteger-cpu-arguments-slope", 812)
                    ,  ("addInteger-memory-arguments-intercept", 1)
                    ,  ("addInteger-memory-arguments-slope", 1)
                    ,  ("appendByteString-cpu-arguments-intercept", 1000)
                    ,  ("appendByteString-cpu-arguments-slope", 571)
                    ,  ("appendByteString-memory-arguments-intercept", 0)
                    ,  ("appendByteString-memory-arguments-slope", 1)
                    ,  ("appendString-cpu-arguments-intercept", 1000)
                    ,  ("appendString-cpu-arguments-slope", 24177)
                    ,  ("appendString-memory-arguments-intercept", 4)
                    ,  ("appendString-memory-arguments-slope", 1)
                    ,  ("bData-cpu-arguments", 1000)
                    ,  ("bData-memory-arguments", 32)
                    ,  ("blake2b-cpu-arguments-intercept", 117366)
                    ,  ("blake2b-cpu-arguments-slope", 10475)
                    ,  ("blake2b-memory-arguments", 4)
                    ,  ("cekApplyCost-exBudgetCPU", 23000)
                    ,  ("cekApplyCost-exBudgetMemory", 100)
                    ,  ("cekBuiltinCost-exBudgetCPU", 23000)
                    ,  ("cekBuiltinCost-exBudgetMemory", 100)
                    ,  ("cekConstCost-exBudgetCPU", 23000)
                    ,  ("cekConstCost-exBudgetMemory", 100)
                    ,  ("cekDelayCost-exBudgetCPU", 23000)
                    ,  ("cekDelayCost-exBudgetMemory", 100)
                    ,  ("cekForceCost-exBudgetCPU", 23000)
                    ,  ("cekForceCost-exBudgetMemory", 100)
                    ,  ("cekLamCost-exBudgetCPU", 23000)
                    ,  ("cekLamCost-exBudgetMemory", 100)
                    ,  ("cekStartupCost-exBudgetCPU", 100)
                    ,  ("cekStartupCost-exBudgetMemory", 100)
                    ,  ("cekVarCost-exBudgetCPU", 23000)
                    ,  ("cekVarCost-exBudgetMemory", 100)
                    ,  ("chooseData-cpu-arguments", 19537)
                    ,  ("chooseData-memory-arguments", 32)
                    ,  ("chooseList-cpu-arguments", 175354)
                    ,  ("chooseList-memory-arguments", 32)
                    ,  ("chooseUnit-cpu-arguments", 46417)
                    ,  ("chooseUnit-memory-arguments", 4)
                    ,  ("consByteString-cpu-arguments-intercept", 221973)
                    ,  ("consByteString-cpu-arguments-slope", 511)
                    ,  ("consByteString-memory-arguments-intercept", 0)
                    ,  ("consByteString-memory-arguments-slope", 1)
                    ,  ("constrData-cpu-arguments", 89141)
                    ,  ("constrData-memory-arguments", 32)
                    ,  ("decodeUtf8-cpu-arguments-intercept", 497525)
                    ,  ("decodeUtf8-cpu-arguments-slope", 14068)
                    ,  ("decodeUtf8-memory-arguments-intercept", 4)
                    ,  ("decodeUtf8-memory-arguments-slope", 2)
                    ,  ("divideInteger-cpu-arguments-constant", 196500)
                    ,  ("divideInteger-cpu-arguments-model-arguments-intercept", 453240)
                    ,  ("divideInteger-cpu-arguments-model-arguments-slope", 220)
                    ,  ("divideInteger-memory-arguments-intercept", 0)
                    ,  ("divideInteger-memory-arguments-minimum", 1)
                    ,  ("divideInteger-memory-arguments-slope", 1)
                    ,  ("encodeUtf8-cpu-arguments-intercept", 1000)
                    ,  ("encodeUtf8-cpu-arguments-slope", 28662)
                    ,  ("encodeUtf8-memory-arguments-intercept", 4)
                    ,  ("encodeUtf8-memory-arguments-slope", 2)
                    ,  ("equalsByteString-cpu-arguments-constant", 245000)
                    ,  ("equalsByteString-cpu-arguments-intercept", 216773)
                    ,  ("equalsByteString-cpu-arguments-slope", 62)
                    ,  ("equalsByteString-memory-arguments", 1)
                    ,  ("equalsData-cpu-arguments-intercept", 1060367)
                    ,  ("equalsData-cpu-arguments-slope", 12586)
                    ,  ("equalsData-memory-arguments", 1)
                    ,  ("equalsInteger-cpu-arguments-intercept", 208512)
                    ,  ("equalsInteger-cpu-arguments-slope", 421)
                    ,  ("equalsInteger-memory-arguments", 1)
                    ,  ("equalsString-cpu-arguments-constant", 187000)
                    ,  ("equalsString-cpu-arguments-intercept", 1000)
                    ,  ("equalsString-cpu-arguments-slope", 52998)
                    ,  ("equalsString-memory-arguments", 1)
                    ,  ("fstPair-cpu-arguments", 80436)
                    ,  ("fstPair-memory-arguments", 32)
                    ,  ("headList-cpu-arguments", 43249)
                    ,  ("headList-memory-arguments", 32)
                    ,  ("iData-cpu-arguments", 1000)
                    ,  ("iData-memory-arguments", 32)
                    ,  ("ifThenElse-cpu-arguments", 80556)
                    ,  ("ifThenElse-memory-arguments", 1)
                    ,  ("indexByteString-cpu-arguments", 57667)
                    ,  ("indexByteString-memory-arguments", 4)
                    ,  ("lengthOfByteString-cpu-arguments", 1000)
                    ,  ("lengthOfByteString-memory-arguments", 10)
                    ,  ("lessThanByteString-cpu-arguments-intercept", 197145)
                    ,  ("lessThanByteString-cpu-arguments-slope", 156)
                    ,  ("lessThanByteString-memory-arguments", 1)
                    ,  ("lessThanEqualsByteString-cpu-arguments-intercept", 197145)
                    ,  ("lessThanEqualsByteString-cpu-arguments-slope", 156)
                    ,  ("lessThanEqualsByteString-memory-arguments", 1)
                    ,  ("lessThanEqualsInteger-cpu-arguments-intercept", 204924)
                    ,  ("lessThanEqualsInteger-cpu-arguments-slope", 473)
                    ,  ("lessThanEqualsInteger-memory-arguments", 1)
                    ,  ("lessThanInteger-cpu-arguments-intercept", 208896)
                    ,  ("lessThanInteger-cpu-arguments-slope", 511)
                    ,  ("lessThanInteger-memory-arguments", 1)
                    ,  ("listData-cpu-arguments", 52467)
                    ,  ("listData-memory-arguments", 32)
                    ,  ("mapData-cpu-arguments", 64832)
                    ,  ("mapData-memory-arguments", 32)
                    ,  ("mkCons-cpu-arguments", 65493)
                    ,  ("mkCons-memory-arguments", 32)
                    ,  ("mkNilData-cpu-arguments", 22558)
                    ,  ("mkNilData-memory-arguments", 32)
                    ,  ("mkNilPairData-cpu-arguments", 16563)
                    ,  ("mkNilPairData-memory-arguments", 32)
                    ,  ("mkPairData-cpu-arguments", 76511)
                    ,  ("mkPairData-memory-arguments", 32)
                    ,  ("modInteger-cpu-arguments-constant", 196500)
                    ,  ("modInteger-cpu-arguments-model-arguments-intercept", 453240)
                    ,  ("modInteger-cpu-arguments-model-arguments-slope", 220)
                    ,  ("modInteger-memory-arguments-intercept", 0)
                    ,  ("modInteger-memory-arguments-minimum", 1)
                    ,  ("modInteger-memory-arguments-slope", 1)
                    ,  ("multiplyInteger-cpu-arguments-intercept", 69522)
                    ,  ("multiplyInteger-cpu-arguments-slope", 11687)
                    ,  ("multiplyInteger-memory-arguments-intercept", 0)
                    ,  ("multiplyInteger-memory-arguments-slope", 1)
                    ,  ("nullList-cpu-arguments", 60091)
                    ,  ("nullList-memory-arguments", 32)
                    ,  ("quotientInteger-cpu-arguments-constant", 196500)
                    ,  ("quotientInteger-cpu-arguments-model-arguments-intercept", 453240)
                    ,  ("quotientInteger-cpu-arguments-model-arguments-slope", 220)
                    ,  ("quotientInteger-memory-arguments-intercept", 0)
                    ,  ("quotientInteger-memory-arguments-minimum", 1)
                    ,  ("quotientInteger-memory-arguments-slope", 1)
                    ,  ("remainderInteger-cpu-arguments-constant", 196500)
                    ,  ("remainderInteger-cpu-arguments-model-arguments-intercept", 453240)
                    ,  ("remainderInteger-cpu-arguments-model-arguments-slope", 220)
                    ,  ("remainderInteger-memory-arguments-intercept", 0)
                    ,  ("remainderInteger-memory-arguments-minimum", 1)
                    ,  ("remainderInteger-memory-arguments-slope", 1)
                    ,  ("sha2_256-cpu-arguments-intercept", 806990)
                    ,  ("sha2_256-cpu-arguments-slope", 30482)
                    ,  ("sha2_256-memory-arguments", 4)
                    ,  ("sha3_256-cpu-arguments-intercept", 1927926)
                    ,  ("sha3_256-cpu-arguments-slope", 82523)
                    ,  ("sha3_256-memory-arguments", 4)
                    ,  ("sliceByteString-cpu-arguments-intercept", 265318)
                    ,  ("sliceByteString-cpu-arguments-slope", 0)
                    ,  ("sliceByteString-memory-arguments-intercept", 4)
                    ,  ("sliceByteString-memory-arguments-slope", 0)
                    ,  ("sndPair-cpu-arguments", 85931)
                    ,  ("sndPair-memory-arguments", 32)
                    ,  ("subtractInteger-cpu-arguments-intercept", 205665)
                    ,  ("subtractInteger-cpu-arguments-slope", 812)
                    ,  ("subtractInteger-memory-arguments-intercept", 1)
                    ,  ("subtractInteger-memory-arguments-slope", 1)
                    ,  ("tailList-cpu-arguments", 41182)
                    ,  ("tailList-memory-arguments", 32)
                    ,  ("trace-cpu-arguments", 212342)
                    ,  ("trace-memory-arguments", 32)
                    ,  ("unBData-cpu-arguments", 31220)
                    ,  ("unBData-memory-arguments", 32)
                    ,  ("unConstrData-cpu-arguments", 32696)
                    ,  ("unConstrData-memory-arguments", 32)
                    ,  ("unIData-cpu-arguments", 43357)
                    ,  ("unIData-memory-arguments", 32)
                    ,  ("unListData-cpu-arguments", 32247)
                    ,  ("unListData-memory-arguments", 32)
                    ,  ("unMapData-cpu-arguments", 38314)
                    ,  ("unMapData-memory-arguments", 32)
                    ,  ("verifySignature-cpu-arguments-intercept", 9462713)
                    ,  ("verifySignature-cpu-arguments-slope", 1021)
                    ,  ("verifySignature-memory-arguments", 10)
                    ]
                )
            )
            ]
    , protocolParamPrices =
            Just (Api.ExecutionUnitPrices { priceExecutionSteps = 721 % 10000000
                                          , priceExecutionMemory = 577 % 10000
                                          }
                 )
    , protocolParamMaxTxExUnits =
            Just (Api.ExecutionUnits { executionSteps = 10000000000
                                     , executionMemory = 16000000
                                     }
                 )
    , protocolParamMaxBlockExUnits =
            Just (Api.ExecutionUnits { executionSteps = 40000000000
                                     , executionMemory = 80000000
                                     }
                 )
    , protocolParamMaxValueSize = Just 5000
    , protocolParamCollateralPercent = Just 150
    , protocolParamMaxCollateralInputs = Just 3
    }

-- CODE STOLEN FROM:
-- https://github.com/input-output-hk/cardano-wallet/blob/06a5b176123c6b540ed1bc8a783953b07bba0a66/lib/core/test-common/Cardano/Wallet/DummyTarget/Primitive/Types.hs

dummyTimeInterpreter :: Monad m => TimeInterpreter m
dummyTimeInterpreter = hoistTimeInterpreter (pure . runIdentity) $
                       mkSingleEraInterpreter
                       (getGenesisBlockDate dummyGenesisParameters)
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
dummyGenesisParameters :: GenesisParameters
dummyGenesisParameters =
    GenesisParameters
    { getGenesisBlockHash = dummyGenesisHash
    , getGenesisBlockDate = StartTime $ posixSecondsToUTCTime 1655683200
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
