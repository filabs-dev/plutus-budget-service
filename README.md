# Budget Service

Execution units evaluate implementation extracted from the cardano-wallet. The server take the JSON file format from the yielded unbalanced
transaction that the PAB returns in the `RemoteWallet` mode. An example of this
file can be found on `example.json`.

## Running the server locally
To run the service first we need to compile from the source. This project does not require nix to be built, if you decide not to use it depending on the OS/Distribution some extra dependencies might be needed.

**For debian/ubuntu:**

```bash
sudo apt install libgmp-dev zlib1g-dev libpq-dev libtinfo-dev libsodium-dev libpq5 pkg-config build-essential
```

**For Arch/Manjaro:**

```bash
sudo pacman -S gmp zlib postgresql-libs ncurses libsodium pkgconf base-devel
```

You will also need a `ghc 8.10.7` and `cabal-install 3.6.2.0` please refer to [ghcup](https://www.haskell.org/ghcup/install/) on how to install

Now we can compile from source:

1. Clone the repository
    - `git clone git@github.com:/joinplank/plutus-budget-service`
2. Build using cabal:
    - `cabal build budget-server`
3. Execute the service:
    - `cabal exec budget-server -- --config [Network configuration file]`

Note: We provide configuration files for mainnet and preprod inside the configurations directory in the repository.****

## Running the server through Docker
You can also run the server through docker.
Building the image only requires you to have docker installed.

You can use the provided Dockerfile to build the docker image by using the following command:

`docker build --tag budget-server:0.1.0.0 .`

After building the image you can run the server using:

`docker run --name budget-server -p 3001:3001 -d budget-server [Network configuration file]`

The default image provides two configuration files, `/configurations/mainnet.json` and `/configurations/preprod.json`. If you wish to provide your own configuration file you need to add it to the image running a command like this one:

`docker run --name budget-server -p 3001:3001 -d -v [Local path to config file]:[Container path to config file] budget-server [Container path to config file]`

Keep in mind that the local path should be the absolute path to the file. It is recommended to use /configurations/{filename}.json as the container path for the config file.

## Usage Example
We can call the endpoint `evaluate` with the json file `example.json` as the body of the
request. For instance running the server locally:
```
$> curl -X POST localhost:3001/evaluate -H 'Content-Type: application/json' -d @example.json
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
