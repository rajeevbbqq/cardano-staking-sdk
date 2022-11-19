module CreatePool (createBondedPoolContract, getBondedPoolsContract) where

import Contract.Prelude

import Contract.Address
  ( Bech32String
  , addressToBech32
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Log (logAesonInfo, logWarn')
import Contract.Monad (Contract, liftContractM, liftedE, liftedE', liftedM)
import Contract.PlutusData
  ( DataHash(..)
  , Datum(Datum)
  , OutputDatum(..)
  , PlutusData
  , datumHash
  , fromData
  , getDatumsByHashes
  , toData
  )
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction
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
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Scripts.StateNFT (mkStateNFTPolicy)
import Settings
  ( bondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types
  ( BondedPoolParams
  , BondedStakingDatum(StateDatum)
  , InitialBondedParams
  , StakingType(Bonded)
  )
import Utils
  ( addressFromBech32
  , logInfo_
  , mkBondedPoolParams
  , mustPayToScript
  , repeatUntilConfirmed
  )

-- Sets up pool configuration, mints the state NFT and deposits
-- in the pool validator's address
createBondedPoolContract
  :: InitialBondedParams
  -> Contract ()
       { txId :: String
       , bondedPoolParams :: BondedPoolParams
       , address :: Bech32String
       }
createBondedPoolContract ibp =
  repeatUntilConfirmed confirmationTimeout submissionAttempts
    do
      adminPkh <- liftedM "createBondedPoolContract: Cannot get admin's pkh"
        ownPaymentPubKeyHash
      logInfo_ "createBondedPoolContract: Admin PaymentPubKeyHash" adminPkh
      -- Get the (Nami) wallet address
      adminAddr <-
        liftedM "createBondedPoolContract: Cannot get wallet Address"
          getWalletAddress
      logInfo_ "createBondedPoolContract: Admin Address"
        =<< addressToBech32 adminAddr
      -- Get utxos at the wallet address
      adminUtxos <- liftedM "createBondedPoolContract: Cannot get user Utxos"
        $ utxosAt adminAddr
      txOutRef <-
        liftContractM "createBondedPoolContract: Could not get head UTXO"
          $ fst
          <$> (Array.head $ toUnfoldable adminUtxos)
      logInfo_ "createBondedPoolContract: Admin Utxos" adminUtxos
      -- Get the minting policy and currency symbol from the state NFT:
      statePolicy <- liftedE $ mkStateNFTPolicy Bonded txOutRef
      stateNftCs <-
        liftContractM
          "createBondedPoolContract: Cannot get CurrencySymbol from state NFT"
          $ scriptCurrencySymbol statePolicy
      -- Get the minting policy and currency symbol from the list NFT:
      listPolicy <- liftedE $ mkListNFTPolicy Bonded stateNftCs
      assocListCs <-
        liftContractM
          "createBondedPoolContract: Cannot get CurrencySymbol from state NFT"
          $ scriptCurrencySymbol listPolicy
      -- May want to hardcode this somewhere:
      tokenName <-
        liftContractM "createBondedPoolContract: Cannot create TokenName"
          bondedStakingTokenName
      -- We define the parameters of the pool
      let
        bondedPoolParams = mkBondedPoolParams adminPkh stateNftCs assocListCs
          ibp
      -- Get the bonding validator and hash
      validator <-
        liftedE' "createBondedPoolContract: Cannot create validator"
          $ mkBondedPoolValidator bondedPoolParams
      let
        valHash = validatorHash validator
        mintValue = singleton stateNftCs tokenName one
      address <- addressToBech32 $ scriptHashAddress valHash
      logInfo_ "createBondedPoolContract: BondedPool Validator's address"
        address
      let
        -- We initalize the pool with no head entry and a pool size of 100_000_000
        bondedStateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Nothing
          }

        lookup :: ScriptLookups.ScriptLookups PlutusData
        lookup = mconcat
          [ ScriptLookups.mintingPolicy statePolicy
          , ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs adminUtxos
          ]

        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustPayToScript valHash bondedStateDatum mintValue
            , mustMintValue mintValue
            , mustSpendPubKeyOutput txOutRef
            ]

      unattachedUnbalancedTx <-
        liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
      logAesonInfo unattachedUnbalancedTx
      -- `balanceAndSignTx` does the following:
      -- 1) Balance a transaction
      -- 2) Reindex `Spend` redeemers after finalising transaction inputs.
      -- 3) Attach datums and redeemers to transaction.
      -- 3) Sign tx, returning the Cbor-hex encoded `ByteArray`.
      signedTx <-
        liftedM
          "createBondedPoolContract: Cannot balance, reindex redeemers, attach /\
          \datums redeemers and sign"
          $ balanceAndSignTx unattachedUnbalancedTx
      -- Return the transaction and the pool info for subsequent transactions
      pure { signedTx, bondedPoolParams, address }

-- Get all the pools at the given address. Although more than one could be
-- returned, in all likelihood the user intended (and managed) to create only
-- one. This is because most pools will have unique start times.
getBondedPoolsContract
  :: String
  -> InitialBondedParams
  -> Contract () (Array BondedPoolParams)
getBondedPoolsContract addrStr ibp = do
  -- Get all UTxOs locked in the protocol's address
  poolUtxos <- liftedM "(getBondedPoolsContract) Could not get pool UTxOs"
    $ utxosAt
    =<< addressFromBech32 addrStr
  logInfo_ "(getBondedPoolContract) UTxOs at pool address: " (show poolUtxos)
  -- For each pool, we obtain its state NFT and assoc list CS (it should be
  -- the only token with name 'BondedStakingToken')
  stateTokenTn <-
    liftMaybe
      ( Exception.error
          "(getBondedPoolsContract) Could not get bonded staking token name"
      )
      bondedStakingTokenName
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
      listPolicy <- liftedE (mkListNFTPolicy Bonded stateNftCs)
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
      "(getBondedPoolsContract) More than one pool with the given address"
  -- For each symbol, we create the bonded params and we returh all of them
  adminPkh <- liftedM "(getBondedPoolsContract) Cannot get admin's pkh"
    ownPaymentPubKeyHash
  pure $ map
    (\(stateCs /\ listCs) -> mkBondedPoolParams adminPkh stateCs listCs ibp)
    symbols
  where
  getValue :: TransactionOutputWithRefScript -> Value
  getValue = _.amount <<< unwrap <<< _.output <<< unwrap
