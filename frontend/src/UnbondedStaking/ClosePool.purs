module UnbondedStaking.ClosePool (closeUnbondedPoolContract) where

import Contract.Prelude

import Contract.Address
  ( getNetworkId
  , getWalletAddress
  , ownPaymentPubKeyHash
  , scriptHashAddress
  )
import Contract.Monad
  ( Contract
  , liftContractM
  , liftContractM
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.Log (logInfo')
import Contract.Numeric.Natural (Natural)
import Contract.Numeric.Rational ((%))
import Contract.PlutusData
  ( Datum(Datum)
  , PlutusData
  , Redeemer(Redeemer)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.Prim.ByteArray (ByteArray)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustIncludeDatum
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, singleton)
import Data.Array (elemIndex, (:), (!!))
import Data.Map (toUnfoldable)
import Plutus.Conversion (fromPlutusAddress)
import Scripts.PoolValidator (mkUnbondedPoolValidator)
import Settings
  ( unbondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types.Scripts (ValidatorHash)
import UnbondedStaking.Types
  ( Entry(Entry)
  , UnbondedPoolParams(UnbondedPoolParams)
  , UnbondedStakingAction(CloseAct)
  , UnbondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  )
import UnbondedStaking.Utils
  ( calculateRewards
  , getAdminTime
  )
import Utils
  ( getUtxoWithNFT
  , mkOnchainAssocList
  , logInfo_
  , mkRatUnsafe
  , roundUp
  , splitByLength
  , submitTransaction
  , toIntUnsafe
  , mustPayToScript
  , getUtxoDatumHash
  )

-- | Closes the unbonded pool and distributes final rewards to users
-- | If the `batchSize` is zero, then funds will be deposited to all users.
-- | Otherwise the transactions will be made in batches
-- | If the `depositList` is empty, then reward deposits will be made to all
-- | users. Otherwise only the users within in the list will have rewards
-- | deposited.
closeUnbondedPoolContract
  :: UnbondedPoolParams
  -> Natural
  -> Array Int
  -> Contract () (Array Int)
closeUnbondedPoolContract
  params@
    ( UnbondedPoolParams
        { admin
        , nftCs
        , assocListCs
        }
    )
  batchSize
  depositList = do
  -- Fetch information related to the pool
  -- Get network ID and check admin's PKH
  networkId <- getNetworkId
  userPkh <- liftedM "closeUnbondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  unless (userPkh == admin) $ throwContractError
    "closeUnbondedPoolContract: Admin is not current user"
  logInfo_ "closeUnbondedPoolContract: Admin PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  adminAddr <-
    liftedM "depositUnbondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  adminUtxos <-
    liftedM "depositUnbondedPoolContract: Cannot get user Utxos" $
      utxosAt adminAddr
  -- Get the unbonded pool validator and hash
  validator <- liftedE' "closeUnbondedPoolContract: Cannot create validator"
    $ mkUnbondedPoolValidator params
  let valHash = validatorHash validator
  logInfo_ "closeUnbondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "closeUnbondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr
  -- Get the unbonded pool's utxo
  unbondedPoolUtxos <-
    liftedM
      "closeUnbondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "closeUnbondedPoolContract: Pool UTXOs" unbondedPoolUtxos
  tokenName <- liftContractM
    "closeUnbondedPoolContract: Cannot create TokenName"
    unbondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "closeUnbondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT unbondedPoolUtxos nftCs tokenName
  logInfo_ "closeUnbondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "closeUnbondedPoolContract: Could not get Pool UTXO's Datum Hash"
      $ getUtxoDatumHash poolTxOutput
  logInfo_ "closeUnbondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "closeUnbondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  unbondedStakingDatum :: UnbondedStakingDatum <-
    liftContractM
      "closeUnbondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  -- Get the bonding range to use
  logInfo' "closeUnbondedPoolContract: Getting admin range..."
  { currTime, range } <- getAdminTime params
  logInfo_ "closeUnbondedPoolContract: Current time: " $ show currTime
  logInfo_ "closeUnbondedPoolContract: TX Range" range
  -- Update the association list
  case unbondedStakingDatum of
    -- Non-empty user list
    StateDatum { maybeEntryName: Just _, open: true } -> do
      logInfo'
        "closeUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ..., open: true }"
      let
        assocList = mkOnchainAssocList assocListCs unbondedPoolUtxos
      -- Concatenate constraints/lookups
      let
        redeemer = Redeemer $ toData CloseAct

        stateDatumConstraintsLookups
          :: Tuple (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
        stateDatumConstraintsLookups =
          ( mustIncludeDatum poolDatum
              <> mustSpendScriptOutput poolTxInput redeemer
          )
            /\ mempty

        constraints :: TxConstraints Unit Unit
        constraints =
          mustBeSignedBy admin
            <> mustIncludeDatum poolDatum
            <> mustValidateIn range

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups =
          ScriptLookups.validator validator
            <> ScriptLookups.unspentOutputs adminUtxos
            <> ScriptLookups.unspentOutputs unbondedPoolUtxos

        submitBatch
          :: Array
               ( Tuple
                   (TxConstraints Unit Unit)
                   (ScriptLookups.ScriptLookups PlutusData)
               )
          -> Contract ()
               ( Array
                   ( Tuple
                       (TxConstraints Unit Unit)
                       (ScriptLookups.ScriptLookups PlutusData)
                   )
               )
        submitBatch txBatch = do
          submitTransaction constraints lookups txBatch confirmationTimeout
            submissionAttempts
      -- Get list of users to deposit rewards too
      updateList <-
        if null depositList then do
          updateList' <- traverse (mkEntryUpdateList params valHash) assocList
          pure $ stateDatumConstraintsLookups : updateList'
        else do
          constraintsLookupsList <-
            traverse (mkEntryUpdateList params valHash) assocList
          updateList' <-
            liftContractM
              "closeUnbondedPoolContract: Failed to create updateList'" $
              traverse ((!!) constraintsLookupsList) depositList
          pure $ stateDatumConstraintsLookups : updateList'
      -- Submit transaction with possible batching
      failedDeposits <-
        if batchSize == zero then
          submitBatch updateList
        else do
          let updateBatches = splitByLength (toIntUnsafe batchSize) updateList
          failedDeposits' <- traverse submitBatch updateBatches
          pure $ mconcat failedDeposits'
      logInfo_
        "closeUnbondedPoolContract: Closed pool and finished updating /\
        \pool entries. Entries with failed updates"
        failedDeposits
      failedDepositsIndicies <-
        liftContractM
          "closeUnbondedPoolContract: Failed to create /\
          \failedDepositsIndicies list" $
          traverse (flip elemIndex updateList) failedDeposits
      pure failedDepositsIndicies
    -- Closing pool with no users
    StateDatum { maybeEntryName: Nothing, open: true } -> do
      logInfo'
        "closeUnbondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Nothing, open: true }"
      -- Bulid constraints/lookups
      let
        redeemer = Redeemer $ toData CloseAct

        constraints :: TxConstraints Unit Unit
        constraints =
          -- Spend all UTXOs to return to Admin along with state/assets
          foldMap
            (flip mustSpendScriptOutput redeemer <<< fst)
            (toUnfoldable unbondedPoolUtxos :: Array _)
            <> mustBeSignedBy admin
            <> mustIncludeDatum poolDatum
            <> mustValidateIn range

        lookups :: ScriptLookups.ScriptLookups PlutusData
        lookups = mconcat
          [ ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs unbondedPoolUtxos
          ]
      failedDeposits <- submitTransaction constraints lookups []
        confirmationTimeout
        submissionAttempts
      logInfo_
        "closeUnbondedPoolContract: Pool closed. Failed updates"
        failedDeposits
      pure
        if null failedDeposits then
          []
        else
          -- After the pool is closed, batching is done by utxo's rather than
          -- user entries. This means the indexing is not needed anymore, and
          -- the caller can just call the ClosePool contract again to spend
          -- the remaining utxos in the pool
          [ zero ]
    -- Other error cases:
    StateDatum { maybeEntryName: _, open: false } ->
      throwContractError
        "closeUnbondedPoolContract: Pool is already closed"
    _ ->
      throwContractError "closeUnbondedPoolContract: Datum incorrect type"

-- | Creates a constraint and lookups list for updating each user entry
mkEntryUpdateList
  :: UnbondedPoolParams
  -> ValidatorHash
  -> (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> Contract ()
       ( Tuple (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
mkEntryUpdateList
  ( UnbondedPoolParams
      { unbondedAssetClass
      , assocListCs
      }
  )
  valHash
  (_ /\ txIn /\ txOut) = do
  -- Get the Entry datum of the old assoc. list element
  dHash <-
    liftContractM
      "mkEntryUpdateList: Could not get Entry Datum Hash"
      $ getUtxoDatumHash txOut
  logInfo_ "mkEntryUpdateList: datum hash" dHash
  listDatum <-
    liftedM
      "mkEntryUpdateList: Cannot get Entry's datum" $ getDatumByHash dHash
  unbondedListDatum :: UnbondedStakingDatum <-
    liftContractM
      "mkEntryUpdateList: Cannot extract NFT State datum"
      $ fromData (unwrap listDatum)
  -- The get the entry datum
  case unbondedListDatum of
    EntryDatum { entry } -> do
      let e = unwrap entry
      calculatedRewards <-
        calculateRewards
          e.rewards
          e.totalRewards
          e.deposited
          e.newDeposit
          e.totalDeposited
      -- Get the token name for the user by hashing
      assocListTn <-
        liftContractM
          "mkEntryUpdateList: Could not create token name for user"
          $ mkTokenName e.key
      -- Update the entry datum
      let
        updatedRewards = roundUp calculatedRewards
        -- Datum and redeemer creation
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry $ e
              { rewards = mkRatUnsafe (updatedRewards % one)
              , open = false
              }
          }
        valRedeemer = Redeemer $ toData CloseAct
        -- Build asset datum and value types
        assetDatum = Datum $ toData AssetDatum
        assetParams = unwrap unbondedAssetClass
        assetCs = assetParams.currencySymbol
        assetTn = assetParams.tokenName
        depositValue = singleton assetCs assetTn updatedRewards
        entryValue = singleton assocListCs assocListTn one

        -- Build constraints and lookups
        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustPayToScript valHash assetDatum depositValue
            , mustPayToScript valHash entryDatum entryValue
            , mustSpendScriptOutput txIn valRedeemer
            ]
      entryDatumLookup <-
        liftContractM
          "mkEntryUpdateList: Could not create state datum lookup"
          $ ScriptLookups.datum entryDatum
      pure (constraints /\ entryDatumLookup)
    _ -> throwContractError
      "mkEntryUpdateList: Datum not Entry constructor"
