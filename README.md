# Budget Service

Execution units estimate implementation extracted from the cardano-wallet. Both
version, the cli and server, take the JSON file format from the yielded unbalanced
transaction that the PAB returns in the `RemoteWallet` mode. An example of this
file can be found on `./script/etx.json`.

## Running the server
We start the server with
```
$> cabal run estimate-server
```
and can call the endpoint `estimate` with the json file as the body of the
request. For instance running the server locally:
```
$> curl -X POST localhost:3001/estimate -H 'Content-Type: application/json' -d @scripts/etx.json
{"Right":{"Spend:3":{"exUnitsMem":1549708,"exUnitsSteps":664250770},"Mint:0":{"exUnitsMem":1396682,"exUnitsSteps":607426316},"Spend:0":{"exUnitsMem":4164373,"exUnitsSteps":1487724013}}}
```

## Possible errors
The first category of errors can be `ErrAssignRedeemers` or `BasicFailure StandardCrypto`:

[Ref](https://github.com/input-output-hk/cardano-wallet/blob/27600f54ce2a4351de2989cf1df88a6087bd1f47/lib/core/src/Cardano/Wallet/Transaction.hs#L499-L514)

```
data ErrAssignRedeemers
    = ErrAssignRedeemersScriptFailure Redeemer String
    -- ^ Failed to assign execution units for a particular redeemer. The
    -- 'String' indicates the reason of the failure.
    --
    -- TODO: Refine this type to avoid the 'String' and provides a better
    -- sum-type of possible errors.
    | ErrAssignRedeemersTargetNotFound Redeemer
    -- ^ The given redeemer target couldn't be located in the transaction.
    | ErrAssignRedeemersInvalidData Redeemer String
    -- ^ Redeemer's data isn't a valid Plutus' data.
    | ErrAssignRedeemersPastHorizon PastHorizonException
    -- ^ Evaluating the Plutus script failed past the visible horizon.
    | ErrAssignRedeemersUnresolvedTxIns [TxIn]
    -- ^ The transaction contains inputs which couldn't be resolved.
    deriving (Generic, Eq, Show)
```

[Ref](https://github.com/input-output-hk/cardano-ledger/blob/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Tools.hs#L52-L55)
```
-- | Basic validtion failures that can be returned by 'evaluateTransactionExecutionUnits'.
data BasicFailure c
  = -- | The transaction contains inputs that are not present in the UTxO.
    UnknownTxIns (Set (TxIn c))
  deriving (Show)
```

Once we dodge these kind of errors we still can have some utxo evaluation script
to fail, if that is the case we are going to receive a `RdmrPtr` associated to
a `ScriptFailure`:

[Ref](https://github.com/input-output-hk/cardano-ledger/blob/1a9ec4ae9e0b09d54e49b2a40c4ead37edadcce5/eras/alonzo/impl/src/Cardano/Ledger/Alonzo/Tools.hs#L58-L81)
```
-- | Script failures that can be returned by 'evaluateTransactionExecutionUnits'.
data ScriptFailure c
  = -- | A redeemer was supplied that does not point to a
    --  valid plutus evaluation site in the given transaction.
    RedeemerNotNeeded RdmrPtr
  | -- | Missing redeemer.
    MissingScript RdmrPtr
  | -- | Missing datum.
    MissingDatum (DataHash c)
  | -- | Plutus V1 evaluation error.
    ValidationFailedV1 PV1.EvaluationError [Text]
  | -- | Plutus V2 evaluation error.
    ValidationFailedV2 PV2.EvaluationError [Text]
  | -- | A redeemer points to a transaction input which is not
    --  present in the current UTxO.
    UnknownTxIn (TxIn c)
  | -- | A redeemer points to a transaction input which is not
    --  plutus locked.
    InvalidTxIn (TxIn c)
  | -- | The execution budget that was calculated by the Plutus
    --  evaluator is out of bounds.
    IncompatibleBudget PV1.ExBudget
  | -- | There was no cost model for given version of Plutus
    NoCostModel Language
  deriving (Show)
```
