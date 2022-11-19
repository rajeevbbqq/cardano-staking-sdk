module UnbondedStaking.CreatePool
  ( createUnbondedPoolContract
  , getUnbondedPoolsContract
  ) where

import Contract.Prelude

import Contract.Address
  ( Bech32String
  , addressToBech32
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logWarn')
import Contract.Monad
  ( Contract
  , liftContractM
  , liftContractM
  , liftedE
  , liftedE'
  , liftedM
  )
import Contract.PlutusData (Datum(Datum), PlutusData, toData)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction
  , TransactionHash
  , TransactionOutputWithRefScript(..)
  , balanceAndSignTx
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustMintValue
  , mustSpendPubKeyOutput
  )
import Contract.Utxos (utxosAt)
import Contract.Value
  ( CurrencySymbol
  , Value
  , flattenValue
  , scriptCurrencySymbol
  , singleton
  )
import Control.Monad.Error.Class (liftMaybe)
import Data.Array as Array
import Data.Map (toUnfoldable)
import Effect.Exception as Exception
import Plutus.Conversion (fromPlutusAddress)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings
  ( confirmationTimeout
  , submissionAttempts
  , unbondedStakingTokenName
  )
import Types (StakingType(Unbonded))
import Types.Interval (POSIXTime(POSIXTime))
import UnbondedStaking.Types
  ( InitialUnbondedParams(InitialUnbondedParams)
  , UnbondedPoolParams
  , UnbondedStakingDatum(StateDatum)
  )
import UnbondedStaking.Utils (mkUnbondedPoolParams)
import Utils
  ( addressFromBech32
  , currentRoundedTime
  , logInfo_
  , mustPayToScript
  , repeatUntilConfirmed
  )

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createUnbondedPoolContract
  :: InitialUnbondedParams
  -> Contract ()
       { txId :: String
       , unbondedPoolParams :: UnbondedPoolParams
       , address :: Bech32String
       }
createUnbondedPoolContract iup =
  repeatUntilConfirmed confirmationTimeout submissionAttempts $ do
    adminPkh <- liftedM "createUnbondedPoolContract: Cannot get admin's pkh"
      ownPaymentPubKeyHash
    logInfo_ "createUnbondedPoolContract: Admin PaymentPubKeyHash" adminPkh
    -- Get the (Nami) wallet address
    adminAddr <-
      liftedM "createUnbondedPoolContract: Cannot get wallet Address"
        getWalletAddress
    logInfo_ "createUnbondedPoolContract: User Address"
      =<< addressToBech32 adminAddr
    -- Get utxos at the wallet address
    adminUtxos <-
      liftedM "createUnbondedPoolContract: Cannot get user Utxos"
        $ utxosAt adminAddr
    txOutRef <-
      liftContractM "createUnbondedPoolContract: Could not get head UTXO"
        $ fst
        <$> (Array.head $ toUnfoldable adminUtxos)
    logInfo_ "createUnbondedPoolContract: Admin Utxos" adminUtxos
    -- Get the minting policy and currency symbol from the state NFT:
    statePolicy <- liftedE $ mkStateNFTPolicy Unbonded txOutRef
    stateNftCs <-
      liftContractM
        "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
        \state NFT"
        $ scriptCurrencySymbol statePolicy
    -- Get the minting policy and currency symbol from the list NFT:
    listPolicy <- liftedE $ mkListNFTPolicy Unbonded stateNftCs
    assocListCs <-
      liftContractM
        "createUnbondedPoolContract: Cannot get CurrencySymbol from /\
        \state NFT"
        $ scriptCurrencySymbol listPolicy
    -- May want to hardcode this somewhere:
    tokenName <-
      liftContractM "createUnbondedPoolContract: Cannot create TokenName"
        unbondedStakingTokenName
    -- We define the parameters of the pool
    let
      unbondedPoolParams = mkUnbondedPoolParams adminPkh stateNftCs assocListCs
        iup
    -- Get the bonding validator and hash
    validator <- liftedE' "createUnbondedPoolContract: Cannot create validator"
      $ mkUnbondedPoolValidator unbondedPoolParams
    let
      valHash = validatorHash validator
      mintValue = singleton stateNftCs tokenName one
    address <- addressToBech32 $ scriptHashAddress valHash
    logInfo_ "createUnbondedPoolContract: UnbondedPool Validator's address"
      address
    let
      unbondedStateDatum = Datum $ toData $ StateDatum
        { maybeEntryName: Nothing
        , open: true
        }

      lookup :: ScriptLookups.ScriptLookups PlutusData
      lookup = mconcat
        [ ScriptLookups.mintingPolicy statePolicy
        , ScriptLookups.validator validator
        , ScriptLookups.unspentOutputs adminUtxos
        ]

      -- Seems suspect, not sure if typed constraints are working as expected
      constraints :: TxConstraints Unit Unit
      constraints =
        mconcat
          [ mustPayToScript valHash unbondedStateDatum mintValue
          , mustMintValue mintValue
          , mustSpendPubKeyOutput txOutRef
          ]

    unattachedBalancedTx <-
      liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints

    -- `balanceAndSignTx` does the following:
    -- 1) Balance a transaction
    -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
    -- 3) Attach datums and redeemers to transaction.
    -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
    signedTx <-
      liftedM
        "createUnbondedPoolContract: Cannot balance, reindex redeemers, attach /\
        \datums redeemers and sign"
        $ balanceAndSignTx unattachedBalancedTx

    -- Return the pool info for subsequent transactions
    pure { signedTx, unbondedPoolParams, address }

-- Get all the pools at the given address. Although more than one could be
-- returned, in all likelihood the user intended (and managed) to create only
-- one. This is because most pools will have unique start times.
getUnbondedPoolsContract
  :: String
  -> InitialUnbondedParams
  -> Contract () (Array UnbondedPoolParams)
getUnbondedPoolsContract addrStr ibp = do
  -- Get all UTxOs locked in the protocol's address
  poolUtxos <- liftedM "(getUnbondedPoolsContract) Could not get pool UTxOs"
    $ utxosAt
    =<< addressFromBech32 addrStr
  logInfo_ "(getUnbondedPoolContract) UTxOs at pool address: " (show poolUtxos)
  -- For each pool, we obtain its state NFT and assoc list CS (it should be
  -- the only token with name 'UnbondedStakingToken')
  stateTokenTn <-
    liftMaybe
      ( Exception.error
          "(getUnbondedPoolsContract) Could not get bonded staking token name"
      )
      unbondedStakingTokenName
  let
    getStateTokenCs :: Value -> Maybe CurrencySymbol
    getStateTokenCs =
      Array.head
        <<< map fst
        <<< Array.filter ((_ == stateTokenTn) <<< fst <<< snd)
        <<< flattenValue

    addListTokenCs
      :: CurrencySymbol -> Contract () (CurrencySymbol /\ CurrencySymbol)
    addListTokenCs stateNftCs = do
      listPolicy <- liftedE (mkListNFTPolicy Unbonded stateNftCs)
      listNftCs <-
        liftMaybe
          (Exception.error "Could not obtain currency symbol from list policy")
          $
            scriptCurrencySymbol listPolicy
      pure $ stateNftCs /\ listNftCs
  symbols <- traverse addListTokenCs
    $ Array.mapMaybe (getStateTokenCs <<< getValue)
    $ Array.fromFoldable poolUtxos
  when (Array.length symbols > 1) $
    logWarn'
      "(getUnbondedPoolsContract) More than one pool with the given address"
  -- For each symbol, we create the bonded params and we returh all of them
  adminPkh <- liftedM "(getUnbondedPoolsContract) Cannot get admin's pkh"
    ownPaymentPubKeyHash
  pure $ map
    (\(stateCs /\ listCs) -> mkUnbondedPoolParams adminPkh stateCs listCs ibp)
    symbols
  where
  getValue :: TransactionOutputWithRefScript -> Value
  getValue = _.amount <<< unwrap <<< _.output <<< unwrap
