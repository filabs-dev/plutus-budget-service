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

module Evaluate where

import           Data.Array                  (Array)
import           Data.Aeson                  hiding (Array)
import           Data.Aeson.Types            hiding (Array)
import qualified Data.ByteString.Char8 as B8
import           Data.Functor.Identity       ( runIdentity )
import qualified Data.Map              as M  (Map, fromList)
import qualified Data.Maybe.Strict     as Strict (StrictMaybe(..))
import           Data.Quantity               (Quantity (..))
import           Data.Time.Clock.POSIX       (posixSecondsToUTCTime, POSIXTime)
import qualified Data.Text             as T
import           Data.Maybe           (fromJust)

import           Control.Arrow              ( left )
import           Control.Monad.Trans.Except ( runExceptT )

import           GHC.Records ( HasField (..) )

import           Plutus.Contract.Wallet ( ExportTx(..), ExportTxInput (..) )

import qualified Cardano.Crypto.Hash.Class as Crypto

import           Cardano.Ledger.Alonzo.Scripts   ( ExUnits )
import           Cardano.Ledger.Alonzo.TxWitness ( RdmrPtr (..) )
import           Cardano.Ledger.Alonzo.Tools (
                                                evaluateTransactionExecutionUnits
                                              , TransactionScriptFailure
                                             )
import qualified Cardano.Ledger.Alonzo.Scripts  as Alonzo
import qualified Cardano.Ledger.Alonzo.Language as Alonzo
import           Cardano.Ledger.Alonzo.TxInfo  (TranslationError(..))
import qualified Cardano.Ledger.Babbage        as Babbage
import qualified Cardano.Ledger.Babbage.TxBody as Babbage
import qualified Cardano.Ledger.Babbage.PParams as Babbage

import qualified Cardano.Ledger.Core     as Core   ( Tx )
import           Cardano.Ledger.Crypto             ( StandardCrypto )
import           Cardano.Ledger.SafeHash           ( unsafeMakeSafeHash

                                                   )
import qualified Cardano.Ledger.Shelley.UTxO as UTxO ( UTxO(..) )
import qualified Cardano.Ledger.TxIn         as LedgerTxIn ( TxIn )

import qualified Cardano.Api         as Api     ( AssetId(..)






                                                , Value
                                                , lovelaceToQuantity
                                                , serialiseToRawBytes
                                                , valueFromList
                                                , toLedgerPParams
                                                )
import qualified Cardano.Api.Shelley as Shelley ( AlonzoEra, TxIn(..)
                                                , BabbageEra
                                                , Tx(ShelleyTx)
                                                , ShelleyLedgerEra
                                                , ProtocolParameters(..)
                                                , ShelleyBasedEra(..)
                                                , toMaryValue
                                                , toShelleyAddr
                                                , toShelleyTxIn
                                                )

import Cardano.Wallet.Transaction ( ErrAssignRedeemers (..) )
import Cardano.Wallet.Shelley.Compatibility (
                                             toCostModelsAsArray
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
--import Cardano.Wallet.Write.Tx        (fromCardanoUTxO)

import Cardano.Slotting.Time          ( SystemStart )
import Cardano.Slotting.EpochInfo.API ( hoistEpochInfo, EpochInfo )

import GHC.Generics (Generic)
import Cardano.Api.Shelley (TxId)
import Cardano.Api (TxIx (TxIx))
import qualified Cardano.Ledger.Alonzo.Scripts as Babbage
import Data.ByteString.Short (pack)
import Data.Word (Word8)

import Data.Char (digitToInt)

data Config = Config {
    protocolParameters :: Shelley.ProtocolParameters,
    genesisTime        :: POSIXTime
} deriving (Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

data EvaluateError = ErrAssign ErrAssignRedeemers
                   | BFailure  (TranslationError StandardCrypto)
    deriving Show

type AlonzoTx = Core.Tx (Shelley.ShelleyLedgerEra Shelley.AlonzoEra)
type BabbageTx = Core.Tx (Shelley.ShelleyLedgerEra Shelley.BabbageEra)
type RedeemerReport = M.Map RdmrPtr
                            (Either (TransactionScriptFailure StandardCrypto) ExUnits)

instance ToJSON EvaluateError where
    toJSON (ErrAssign errAssign) = object ["ErrAssign" .= show errAssign]
    toJSON (BFailure bFail)      = object ["BFailure" .= show bFail]

instance ToJSON (TransactionScriptFailure StandardCrypto) where
    toJSON a = object ["ScriptFail" .= show a]

instance ToJSON RdmrPtr where
    toJSON (RdmrPtr tag w) = object ["tag" .= show tag, "index" .= show w]

instance ToJSONKey RdmrPtr where
    toJSONKey = toJSONKeyText
                (\(RdmrPtr tag w) -> T.pack $ show tag ++ ":" ++ show w)

instance {-# OVERLAPPING #-}
    ToJSON (Either (TransactionScriptFailure StandardCrypto) ExUnits) where
    toJSON (Left err) = toJSON err
    toJSON (Right eu) = toJSON eu

evaluate
    :: Config
    -> ExportTx
    -> Either EvaluateError RedeemerReport
evaluate conf ExportTx{..} =
    mkEpochInfo conf >>= \epochInfo ->
    case
        evaluateTransactionExecutionUnits
        pparams
        babbageTx
        utxos
        epochInfo
        (systemStart conf)
        costs
    of
        Left err -> Left $ BFailure err
        Right redRep -> Right redRep
  where
    babbageTx :: BabbageTx
    (Shelley.ShelleyTx _ babbageTx) = partialTx

    -- ppa64rams :: Core.PParams (Babbage.BabbageEra StandardCrypto)
    pparams :: Babbage.PParams (Babbage.BabbageEra StandardCrypto)
    pparams = Api.toLedgerPParams Shelley.ShelleyBasedEraBabbage $ protocolParameters conf

    utxos :: UTxO.UTxO (Shelley.ShelleyLedgerEra Shelley.BabbageEra)
    utxos = utxosFromExportTxInputs lookups

    costs :: Array Alonzo.Language Alonzo.CostModel
    costs = toCostModelsAsArray (Alonzo.unCostModels $ getField @"_costmdls" pparams)

ti :: Config -> TimeInterpreter (Either PastHorizonException)
ti = dummyTimeInterpreter

systemStart :: Config -> SystemStart
systemStart = getSystemStart . ti

mkEpochInfo :: Config -> Either EvaluateError (EpochInfo (Either T.Text))
mkEpochInfo conf =
    hoistEpochInfo
        (left
            (T.pack . show . ErrAssign
            . ErrAssignRedeemersTranslationError
            . TimeTranslationPastHorizon . T.pack . show )
            . runIdentity . runExceptT)
    <$>
    left (ErrAssign
        . ErrAssignRedeemersTranslationError
        . TimeTranslationPastHorizon . T.pack . show ) (toEpochInfo . ti $ conf)

utxosFromExportTxInputs
    :: [ExportTxInput]
    -> UTxO.UTxO (Shelley.ShelleyLedgerEra Shelley.BabbageEra)
utxosFromExportTxInputs insInfo =
    UTxO.UTxO $
    M.fromList [ utxoFromInputInfo input | input <- insInfo ]

utxoFromInputInfo
    :: ExportTxInput
    -> ( LedgerTxIn.TxIn StandardCrypto
       , Babbage.TxOut (Babbage.BabbageEra StandardCrypto)
       )
utxoFromInputInfo ExportTxInput{..} = (utxoTxIn, utxoTxOut)
  where
    txid :: TxId
    txid = "7e223c83171cbb386796f26f62fc1aa702e858eab15b8abb8c7dc77555d903a2"
    txix :: TxIx
    txix = TxIx 0


    utxoTxIn :: LedgerTxIn.TxIn StandardCrypto
    utxoTxIn = Shelley.toShelleyTxIn $ Shelley.TxIn etxiId etxiTxIx

    utxoTxOut :: Babbage.TxOut (Babbage.BabbageEra StandardCrypto)
    utxoTxOut =
        Babbage.TxOut
        (Shelley.toShelleyAddr etxiAddress)
        (Shelley.toMaryValue val)
        mDatumHash
        (if etxiId == txid && etxiTxIx == txix
        then
            Strict.SJust script
        else
            Strict.SNothing)

    val :: Api.Value
    val = Api.valueFromList $
          (Api.AdaAssetId, Api.lovelaceToQuantity etxiLovelaceQuantity) :
          [(Api.AssetId id' an, q) | (id', an, q) <- etxiAssets]


    mDatumHash :: Babbage.Datum (Babbage.BabbageEra StandardCrypto)
    mDatumHash =
        case etxiDatumHash of
            Nothing -> Babbage.NoDatum
            Just dh -> Babbage.DatumHash $
                       unsafeMakeSafeHash $ fromJust $ Crypto.hashFromBytes
                       $ Api.serialiseToRawBytes dh
        -- maybeToStrictMaybe $
        -- castSafeHash . unsafeMakeSafeHash <$>
        -- (etxiDatumHash >>= Crypto.hashFromBytes . Api.serialiseToRawBytes)

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

script :: Babbage.Script (Babbage.BabbageEra StandardCrypto)
script = Babbage.PlutusScript Alonzo.PlutusV2 $ pack $ hexToWord8s scriptStr

hexToWord8s :: String -> [Word8]
hexToWord8s [] = []
hexToWord8s (x:y:xs) = word8 : hexToWord8s xs
  where word8 = fromIntegral $ digitToInt x * 16 + digitToInt y
hexToWord8s _ = error "Invalid hex string"

scriptStr :: String
scriptStr = "5911a201000033232323232323232323232323232332232323232322232232322323253353330093333573466e1cd55cea803a4000464646666ae68cdc39aab9d5001480008dd69aba135573ca004464c6403e66ae7008007c0744dd50009aba135573ca010464c6403866ae70074070068cccd5cd19b875004480188488880108cccd5cd19b875005480108c848888c004014c8c8c8cccd5cd19b8735573aa0049000119aa8119bae35742a0046eb8d5d09aba2500223263202133573804404203e26aae7940044dd50009aba135573ca00e46666ae68cdc3a8032400442444400646666ae68cdc3a803a4000424444004464c6403e66ae7008007c07407006c068cccd5cd19b8735573aa0049000119910919800801801191919191919191919191919191999ab9a3370e6aae754031200023333333333332222222222221233333333333300100d00c00b00a00900800700600500400300233501901a35742a01866a0320346ae85402ccd406406cd5d0a805199aa80ebae501c35742a012666aa03aeb94070d5d0a80419a80c8121aba150073335501d02575a6ae854018c8c8c8cccd5cd19b8735573aa00490001199109198008018011919191999ab9a3370e6aae754009200023322123300100300233502f75a6ae854008c0c0d5d09aba2500223263203233573806606406026aae7940044dd50009aba150023232323333573466e1cd55cea8012400046644246600200600466a05eeb4d5d0a80118181aba135744a004464c6406466ae700cc0c80c04d55cf280089baa001357426ae8940088c98c80b8cd5ce01781701609aab9e5001137540026ae854014cd4065d71aba150043335501d021200135742a006666aa03aeb88004d5d0a80118119aba135744a004464c6405466ae700ac0a80a04d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135744a00226ae8940044d5d1280089aba25001135573ca00226ea8004d5d0a80118099aba135744a004464c6403866ae70074070068406c4c98c806ccd5ce2481035054350001b135573ca00226ea80044d55cea80089baa001137540022464460046eb0004c8004d5405488cccd55cf80092805119a80498021aba1002300335744004028464646666ae68cdc39aab9d5002480008cc8848cc00400c008c030d5d0a80118029aba135744a004464c6402866ae700540500484d55cf280089baa0012323232323333573466e1cd55cea8022400046666444424666600200a0080060046464646666ae68cdc39aab9d5002480008cc8848cc00400c008c054d5d0a80119a80780a1aba135744a004464c6403266ae7006806405c4d55cf280089baa00135742a008666aa010eb9401cd5d0a8019919191999ab9a3370ea0029002119091118010021aba135573ca00646666ae68cdc3a80124004464244460020086eb8d5d09aab9e500423333573466e1d400d20002122200323263201b33573803803603203002e26aae7540044dd50009aba1500233500b75c6ae84d5d1280111931900a99ab9c016015013135744a00226ae8940044d55cf280089baa0011335500175ceb44488c88c008dd5800990009aa80911191999aab9f0022500823350073355015300635573aa004600a6aae794008c010d5d100180909aba100111220021221223300100400312232323333573466e1d4005200023212230020033005357426aae79400c8cccd5cd19b8750024800884880048c98c8040cd5ce00880800700689aab9d500113754002464646666ae68cdc3a800a400c46424444600800a600e6ae84d55cf280191999ab9a3370ea004900211909111180100298049aba135573ca00846666ae68cdc3a801a400446424444600200a600e6ae84d55cf280291999ab9a3370ea00890001190911118018029bae357426aae7940188c98c8040cd5ce00880800700680600589aab9d500113754002464646666ae68cdc39aab9d5002480008cc8848cc00400c008c014d5d0a8011bad357426ae8940088c98c8030cd5ce00680600509aab9e5001137540024646666ae68cdc39aab9d5001480008dd71aba135573ca004464c6401466ae7002c0280204dd5000919191919191999ab9a3370ea002900610911111100191999ab9a3370ea004900510911111100211999ab9a3370ea00690041199109111111198008048041bae35742a00a6eb4d5d09aba2500523333573466e1d40112006233221222222233002009008375c6ae85401cdd71aba135744a00e46666ae68cdc3a802a400846644244444446600c01201060186ae854024dd71aba135744a01246666ae68cdc3a8032400446424444444600e010601a6ae84d55cf280591999ab9a3370ea00e900011909111111180280418071aba135573ca018464c6402666ae7005004c04404003c03803403002c4d55cea80209aab9e5003135573ca00426aae7940044dd50009191919191999ab9a3370ea002900111999110911998008028020019bad35742a0086eb4d5d0a8019bad357426ae89400c8cccd5cd19b875002480008c8488c00800cc020d5d09aab9e500623263200c33573801a01801401226aae75400c4d5d1280089aab9e500113754002464646666ae68cdc3a800a400446424460020066eb8d5d09aab9e500323333573466e1d400920002321223002003375c6ae84d55cf280211931900499ab9c00a009007006135573aa00226ea8004488c8c8cccd5cd19b87500148010848880048cccd5cd19b875002480088c84888c00c010c018d5d09aab9e500423333573466e1d400d20002122200223263200a33573801601401000e00c26aae7540044dd50009191999ab9a3370ea0029001109100111999ab9a3370ea0049000109100091931900319ab9c007006004003135573a6ea800526120014910350543100112212330010030021123230010012233003300200200133232332233223232323233223232323232323232323232323232323233223232323232323232222232323232323253333500815335333573466e1ccdc3004a400890010160158816099ab9c49011c5374617465206e6f742076616c696420666f7220636c6f73696e672e0002b15335300c007213500122350012222533533301601400335500922220031500815335335738920115496e76616c6964206f75747075742076616c75652e0003215008103213562615335300c007213500122350012222533533301601400335500b22220031500a1533533573892115496e76616c6964206f75747075742076616c75652e000321500a1032135626232323232153353232325335333573466e20044cdc0000a40080660682a0042a66a666ae68cdc480899b81001480100d00cc5400840ccd4044880044ccd5cd19b883322333355002323350252233350230030010023502000133502422230033002001200122337000029001000a40006038240026605ea0680029003019018a8058a8008a99a99ab9c491225374617465206e6f742076616c696420666f72206d696e74696e67207072697a652e000301500110301533533301301132323355301b120012350012233550380023355301e1200123500122335503b00233350012330354800000488cc0d80080048cc0d40052000001330160020015004500a35500922220031500115335335738920115496e76616c6964206f75747075742076616c75652e0002f15001102f15335333573466e1c030d5402088894ccd4008540b8854cd4c09400484004540bc854cd4cc058004d403c88008854cd4c09800484004540c0540bc0bc0b85400454cd4cd5ce24915496e76616c6964206f75747075742073746174652e0002e15001102e1533533301100f5001350092235002222222222222008102e133573892011e546865207072697a65206973206e6f74206265696e67206d696e7465642e0002d13500122335030335503200233503033550320014800940c540c454cd4ccd5cd19b8753333500710081337020109001099b800084800884024d5400488894ccd400854090854cd4c0840048400454094854cd4cc048004d402c88008854cd4c0880048400454098540940ac0a840ac4cd5ce24915496e76616c6964206f75747075742073746174652e0002a1533530090051301e4988854cd40044008884c0892615335333573466e1d4cccd401440184cdc080324004266e0001920022100735500122225333500215024215335301f0012100115025215335330100013500922002215335302000121001150261502502902810291335738920115496e76616c6964206f75747075742073746174652e000281533530070031301e4988854cd40044008884c089261533530060021301f4988854cd40044008884c08d2615335300600121350012235001222200313562625335300200121350012235001222235008223500222222222222233355301d120012235002222253353501822350062232335005233500425335333573466e3c00800411010c5400c410c810c8cd4010810c94cd4ccd5cd19b8f002001044043150031043153350032153350022133500223350022335002233500223303400200120462335002204623303400200122204622233500420462225335333573466e1c01800c12412054cd4ccd5cd19b8700500204904813302a00400110481048104115335001210411041133503e0060051005503900a132632015335738921024c660001523500122350022222222222223333500d250292502925029233355301a1200133502022533500221003100150292350012253355335333573466e3cd400888008d4010880080cc0c84ccd5cd19b8735002220013500422001033032103213502d0031502c00d22333573466e1c0080040840808c88d4004888888888888c038008c8004d5408c894cd400454068884d4008894cd4ccd5cd19b8f00200702502413501f0011300600322233355300a120013500f500e2350012233355300d1200135012501123500122333500123300a4800000488cc02c0080048cc028005200000133004002001223355300712001235001223355024002333500123355300b1200123500122335502800235500d0010012233355500801000200123355300b1200123500122335502800235500c00100133355500300b00200111122233355300412001501f335530071200123500122335502400235500900133355300412001223500222533533355300c1200132335013223335003220020020013500122001123300122533500210251001022235001223300a002005006100313350230040035020001335530071200123500122323355025003300100532001355025225335001135500a003221350022253353300c002008112223300200a004130060030023200135501e221122253350011002221330050023335530071200100500400111212223003004112122230010043200135501b22112253350011501d22133501e300400233553006120010040013200135501a22112225335001135006003221333500900530040023335530071200100500400112350012200112350012200222333573466e3c008004054050448cc004894cd40084004405004c48cd400888ccd400c88008008004d40048800448848cc00400c0088c8c8ccccccd5d200191999ab9a3370e6aae75400d2000233335573ea0064a01c46666aae7cd5d128021299a9919191999999aba400323333573466e1cd55cea801a400046666aae7d400c940548cccd55cf9aba250042533532333333357480024a0304a0304a03046a0326eb400894060044d5d0a802909a80c0008a80b1280b0078071280a0061280992809928099280980609aab9e5001137540026ae85401484d40440045403c9403c02001c94034014940309403094030940300144d55cf280089baa001498480048d58984d58988d58984d58988d589848488c00800c44880044d589888cdc0001000990009aa803911299a80088011109a8011119803999804001003000801990009aa8031111299a80088011109a80111299a999ab9a3370e00290000050048999804003803001899980400399a80589199800804001801003001891001091000889100109109119800802001889109198008018010891918008009119801980100100099a89119a8911980119aa8022451ca8979b85687e3af04969b8be28790c8ded66129bfd818cc8b361156700488100480608848cc00400c0088004448848cc00400c0084480041"
