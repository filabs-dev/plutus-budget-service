{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Config where

import Cardano.Api.Shelley qualified as Shelley ( ProtocolParameters
                                                , AlonzoEra, ShelleyLedgerEra )
import Cardano.Ledger.Core qualified as Core    ( Tx )
import Cardano.Ledger.Alonzo.Scripts            ( ExUnits )
import Cardano.Ledger.Alonzo.Tools              ( BasicFailure, ScriptFailure )
import Cardano.Ledger.Alonzo.TxWitness          ( RdmrPtr(..) )
import Cardano.Ledger.Crypto                    ( StandardCrypto )
import Cardano.Ledger.Shelley.Rules.Rupd        ( Identity(..) )
import Cardano.Slotting.EpochInfo.API           ( hoistEpochInfo, EpochInfo )
import Cardano.Wallet.Primitive.Slotting        ( TimeInterpreter
                                                , PastHorizonException
                                                , getSystemStart
                                                , toEpochInfo
                                                , hoistTimeInterpreter
                                                , mkSingleEraInterpreter
                                                )
import Cardano.Wallet.Primitive.Types           ( GenesisParameters (..)
                                                , SlottingParameters(..)
                                                , StartTime (..)
                                                , SlotLength (..)
                                                , EpochLength (..)
                                                , ActiveSlotCoefficient (..)
                                                )
import Cardano.Wallet.Primitive.Types.Hash      ( Hash(..) )
import Cardano.Wallet.Transaction               ( ErrAssignRedeemers (..) )
import Cardano.Slotting.Time                    ( SystemStart )
import Control.Arrow                            ( ArrowChoice(..) )
import Control.Monad.Trans.Except               ( runExceptT )
import Data.Aeson                               ( FromJSON(..)
                                                , ToJSON(..)
                                                , ToJSONKey(..)
                                                , object
                                                , (.=)
                                                )
import Data.Map qualified as M                  ( Map )
import Data.Text qualified as T
import Data.ByteString.Char8 qualified as B8
import Data.Time.Clock.POSIX                    ( POSIXTime
                                                , posixSecondsToUTCTime
                                                )
import Data.Aeson.Types                         ( toJSONKeyText )
import Data.Quantity                            ( Quantity (..) )
import GHC.Generics                             ( Generic )

data Config = Config {
    protocolParameters :: Shelley.ProtocolParameters,
    genesisTime        :: POSIXTime
} deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data EvaluateError = ErrAssign ErrAssignRedeemers
                   | BFailure  (BasicFailure StandardCrypto)
    deriving Show

type AlonzoTx = Core.Tx (Shelley.ShelleyLedgerEra Shelley.AlonzoEra)
type RedeemerReport = M.Map RdmrPtr
                            (Either (ScriptFailure StandardCrypto) ExUnits)

instance ToJSON EvaluateError where
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


systemStart :: Config -> SystemStart
systemStart = getSystemStart . dummyTimeInterpreter

mkEpochInfo :: Config -> Either EvaluateError (EpochInfo (Either EvaluateError))
mkEpochInfo conf =
    hoistEpochInfo
    (left (ErrAssign . ErrAssignRedeemersPastHorizon) . runIdentity . runExceptT)
    <$>
    left (ErrAssign . ErrAssignRedeemersPastHorizon) (toEpochInfo . dummyTimeInterpreter $ conf)

-- The dummy functions where copied from here:
-- https://github.com/input-output-hk/cardano-wallet/blob/06a5b176123c6b540ed1bc8a783953b07bba0a66/lib/core/test-common/Cardano/Wallet/DummyTarget/Primitive/Types.hs

dummyTimeInterpreter :: Config -> TimeInterpreter (Either PastHorizonException)
dummyTimeInterpreter conf = hoistTimeInterpreter (pure . runIdentity) $
                            mkSingleEraInterpreter
                            (getGenesisBlockDate $ dummyGenesisParameters conf)
                            dummySlottingParameters

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
