module UserStake (userStakeBondedPoolContract) where

import Contract.Prelude hiding (length)

import BondedStaking.TimeUtils (getStakingTime)
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
  , liftedE
  , liftedE'
  , liftedM
  , throwContractError
  )
import Contract.Log
  ( logInfo'
  , logAesonInfo
  )
import Contract.Numeric.Natural (Natural, toBigInt)
import Contract.PlutusData
  ( PlutusData
  , Datum(Datum)
  , fromData
  , getDatumByHash
  , toData
  )
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (validatorHash)
import Contract.Transaction
  ( BalancedSignedTransaction
  , balanceAndSignTx
  , TransactionHash
  )
import Contract.TxConstraints
  ( TxConstraints
  , mustBeSignedBy
  , mustMintValueWithRedeemer
  , mustSpendScriptOutput
  , mustValidateIn
  )
import Contract.Utxos (utxosAt)
import Contract.Value (mkTokenName, singleton)
import Control.Applicative (unless)
import Data.Array (head)
import Plutus.Conversion (fromPlutusAddress)
import Scripts.ListNFT (mkListNFTPolicy)
import Scripts.PoolValidator (mkBondedPoolValidator)
import Settings
  ( bondedStakingTokenName
  , confirmationTimeout
  , submissionAttempts
  )
import Types
  ( BondedStakingAction(StakeAct)
  , BondedStakingDatum(AssetDatum, EntryDatum, StateDatum)
  , BondedPoolParams(BondedPoolParams)
  , Entry(Entry)
  , ListAction(ListInsert)
  , MintingAction(MintHead)
  , StakingType(Bonded)
  )
import Types.Redeemer (Redeemer(Redeemer))
import Utils
  ( findInsertUpdateElem
  , getUtxoWithNFT
  , hashPkh
  , logInfo_
  , mkOnchainAssocList
  , repeatUntilConfirmed
  , mustPayToScript
  , getUtxoDatumHash
  )

-- Deposits a certain amount in the pool
userStakeBondedPoolContract
  :: BondedPoolParams
  -> Natural
  -> Contract ()
       { txId :: String }
userStakeBondedPoolContract
  params@
    ( BondedPoolParams
        { minStake
        , maxStake
        , bondedAssetClass
        , nftCs
        , assocListCs
        }
    )
  amt = repeatUntilConfirmed confirmationTimeout submissionAttempts do
  -- Fetch information related to the pool
  -- Get network ID
  networkId <- getNetworkId
  userPkh <- liftedM "userStakeBondedPoolContract: Cannot get user's pkh"
    ownPaymentPubKeyHash
  logInfo_ "userStakeBondedPoolContract: User's PaymentPubKeyHash" userPkh
  -- Get the (Nami) wallet address
  userAddr <-
    liftedM "userStakeBondedPoolContract: Cannot get wallet Address"
      getWalletAddress
  -- Get utxos at the wallet address
  userUtxos <-
    liftedM "userStakeBondedPoolContract: Cannot get user Utxos"
      $ utxosAt userAddr
  logInfo_ "userStakeBondedPoolContract: User Address" userAddr
  -- Get the bonded pool validator and hash
  validator <- liftedE' "userStakeBondedPoolContract: Cannot create validator"
    $ mkBondedPoolValidator params
  let valHash = validatorHash validator
  logInfo_ "userStakeBondedPoolContract: validatorHash" valHash
  let poolAddr = scriptHashAddress valHash
  logInfo_ "userStakeBondedPoolContract: Pool address"
    $ fromPlutusAddress networkId poolAddr
  -- Get the bonded pool's utxo
  bondedPoolUtxos <-
    liftedM
      "userStakeBondedPoolContract: Cannot get pool's utxos at pool address"
      $ utxosAt poolAddr
  logInfo_ "userStakeBondedPoolContract: Pool UTXOs" bondedPoolUtxos
  tokenName <- liftContractM
    "userStakeBondedPoolContract: Cannot create TokenName"
    bondedStakingTokenName
  poolTxInput /\ poolTxOutput <-
    liftContractM "userStakeBondedPoolContract: Cannot get state utxo"
      $ getUtxoWithNFT bondedPoolUtxos nftCs tokenName
  logInfo_ "userStakeBondedPoolContract: Pool's UTXO" poolTxInput
  poolDatumHash <-
    liftContractM
      "userStakeBondedPoolContract: Could not get Pool UTXO's Datum Hash"
      $ getUtxoDatumHash poolTxOutput
  logInfo_ "userStakeBondedPoolContract: Pool's UTXO DatumHash" poolDatumHash
  poolDatum <- liftedM "userStakeBondedPoolContract: Cannot get datum"
    $ getDatumByHash poolDatumHash
  bondedStakingDatum :: BondedStakingDatum <-
    liftContractM
      "userStakeBondedPoolContract: Cannot extract NFT State datum"
      $ fromData (unwrap poolDatum)
  hashedUserPkh <- liftAff $ hashPkh userPkh
  let
    amtBigInt = toBigInt amt
    assetDatum = Datum $ toData AssetDatum
    stateTokenValue = singleton nftCs tokenName one
    assetParams = unwrap bondedAssetClass
    assetCs = assetParams.currencySymbol
    assetTn = assetParams.tokenName
    stakeValue = singleton assetCs assetTn amtBigInt
  -- Get the minting policy and currency symbol from the list NFT:
  listPolicy <- liftedE $ mkListNFTPolicy Bonded nftCs
  -- Get the token name for the user by hashing
  assocListTn <-
    liftContractM
      "userStakeBondedPoolContract: Could not create token name for user`"
      $ mkTokenName hashedUserPkh
  let entryValue = singleton assocListCs assocListTn one

  -- Get the staking range to use
  logInfo' "userStakeBondedPoolContract: Getting staking range..."
  { currTime, range } <- getStakingTime params
  logInfo_ "userStakeBondedPoolContract: Current time: " $ show currTime
  logInfo_ "userStakeBondedPoolContract: TX Range" range

  constraints /\ lookup <- case bondedStakingDatum of
    StateDatum { maybeEntryName: Nothing } -> do
      logInfo'
        "userStakeBondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Nothing }"
      -- If the state UTXO has nothing, it is a head deposit, spending the state
      -- UTXO. It's the first stake so we just need to check amt is inside
      -- bounds.
      unless (minStake <= amt && amt <= maxStake)
        $ throwContractError
            "userStakeBondedPoolContract: Stake amount outside of min/max range"
      let
        mh = MintHead poolTxInput
        -- Minting a new Entry
        valRedeemer = Redeemer $ toData $ StakeAct
          { stakeAmount: amt
          , stakeHolder: userPkh
          , mintingAction: Just mh
          }
        mintRedeemer = Redeemer $ toData $ ListInsert mh
        -- Updated bonded state datum
        bondedStateDatum = Datum $ toData $ StateDatum
          { maybeEntryName: Just hashedUserPkh
          }
        -- The new Entry
        entryDatum = Datum $ toData $ EntryDatum
          { entry: Entry
              { key: hashedUserPkh
              , newDeposit: amtBigInt
              , deposited: amtBigInt
              , staked: zero
              , rewards: zero
              , next: Nothing -- There are no other elements in the list
              }
          }

      bondedStateDatumLookup <-
        liftContractM
          "userStakeBondedPoolContract: Could not create state datum lookup"
          $ ScriptLookups.datum bondedStateDatum
      entryDatumLookup <-
        liftContractM
          "userStakeBondedPoolContract: Could not create entry datum lookup"
          $ ScriptLookups.datum entryDatum
      let
        constraints :: TxConstraints Unit Unit
        constraints =
          mconcat
            [ mustMintValueWithRedeemer mintRedeemer entryValue
            , mustPayToScript valHash bondedStateDatum stateTokenValue
            , mustPayToScript valHash assetDatum stakeValue
            , mustPayToScript valHash entryDatum entryValue
            , mustBeSignedBy userPkh
            , mustSpendScriptOutput poolTxInput valRedeemer
            , mustValidateIn range
            ]

        lookup :: ScriptLookups.ScriptLookups PlutusData
        lookup = mconcat
          [ ScriptLookups.mintingPolicy listPolicy
          , ScriptLookups.validator validator
          , ScriptLookups.unspentOutputs userUtxos
          , ScriptLookups.unspentOutputs bondedPoolUtxos
          , bondedStateDatumLookup
          , entryDatumLookup
          ]
      pure $ constraints /\ lookup
    -- Here, the list is non empty:
    StateDatum { maybeEntryName: Just key } -> do
      logInfo'
        "userStakeBondedPoolContract: STAKE TYPE - StateDatum is \
        \StateDatum { maybeEntryName: Just ... }"
      let assocList = mkOnchainAssocList assocListCs bondedPoolUtxos
      -- If hashedUserPkh < key, we have a head deposit, spending the state utxo
      -- If hashedUserPkh == key, this is a non-init deposit spending the first
      -- assoc. list element.
      -- If hashedUserPkh > key, we filter elements and find the last suitable
      -- one.
      case compare hashedUserPkh key of
        LT -> do
          logInfo' "userStakeBondedPoolContract: Compare LT"
          unless (minStake <= amt && amt <= maxStake)
            $ throwContractError
                "userStakeBondedPoolContract: Stake amount outside of min/max \
                \range"
          -- Minting a new Entry
          let
            mh = MintHead poolTxInput
            -- Minting a new Entry
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction: Just mh
              }
            mintRedeemer = Redeemer $ toData $ ListInsert mh
            -- Updated bonded state datum
            bondedStateDatum = Datum $ toData $ StateDatum
              { maybeEntryName: Just hashedUserPkh -- points to new head
              }
            -- The new Entry
            entryDatum = Datum $ toData $ EntryDatum
              { entry: Entry
                  { key: hashedUserPkh
                  , newDeposit: amtBigInt
                  , deposited: amtBigInt
                  , staked: zero
                  , rewards: zero
                  , next: Just key -- points to the previous head.
                  }
              }

          bondedStateDatumLookup <-
            liftContractM
              "userStakeBondedPoolContract: Could not create state datum lookup"
              $ ScriptLookups.datum bondedStateDatum
          entryDatumLookup <-
            liftContractM
              "userStakeBondedPoolContract: Could not create entry datum lookup"
              $ ScriptLookups.datum entryDatum
          let
            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustMintValueWithRedeemer mintRedeemer entryValue
                , mustPayToScript valHash bondedStateDatum stateTokenValue
                , mustPayToScript valHash assetDatum stakeValue
                , mustPayToScript valHash entryDatum entryValue
                , mustBeSignedBy userPkh
                , mustSpendScriptOutput poolTxInput valRedeemer
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.mintingPolicy listPolicy
              , ScriptLookups.validator validator
              , ScriptLookups.unspentOutputs userUtxos
              , ScriptLookups.unspentOutputs bondedPoolUtxos
              , bondedStateDatumLookup
              , entryDatumLookup
              ]
          pure $ constraints /\ lookup
        EQ -> do
          -- If we have equality, the on chain element must already exist, so
          -- we must spend and update it. In this case, we are updating the
          -- head of the list.
          logInfo' "userStakeBondedPoolContract: Compare EQ"
          assocElem <-
            liftContractM
              "userStakeBondedPoolContract: Cannot \
              \extract head from Assoc. List - this should be impossible"
              $ head assocList
          let
            txIn /\ txOut = snd assocElem
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction: Nothing -- equality means we are updating the
              -- head Assoc. List element.
              }
          -- Get the Entry datum of the old assoc. list element
          dHash <-
            liftContractM
              "userStakeBondedPoolContract: Could not get Entry Datum Hash"
              $ getUtxoDatumHash txOut
          logInfo_ "userStakeBondedPoolContract: " dHash
          listDatum <-
            liftedM
              "userStakeBondedPoolContract: Cannot get Entry's\
              \ datum" $ getDatumByHash dHash
          bondedListDatum :: BondedStakingDatum <-
            liftContractM
              "userStakeBondedPoolContract: Cannot extract NFT State datum"
              $ fromData (unwrap listDatum)
          -- The updated Entry Datum
          entryDatum <- case bondedListDatum of
            EntryDatum { entry } -> do
              let
                e = unwrap entry
                updateDeposited = e.deposited + amtBigInt
              unless
                ( toBigInt minStake <= updateDeposited
                    && updateDeposited
                    <= toBigInt maxStake
                )
                $ throwContractError
                    "userStakeBondedPoolContract: Stake amount outside of \
                    \min/max range"
              pure $ Datum $ toData $ EntryDatum
                { entry: Entry $ e
                    { newDeposit = e.newDeposit + amtBigInt
                    , deposited = updateDeposited
                    }
                }
            _ -> throwContractError
              "userStakeBondedPoolContract: Datum not \
              \Entry constructor"

          entryDatumLookup <-
            liftContractM
              "userStakeBondedPoolContract: Could not create state datum lookup"
              $ ScriptLookups.datum entryDatum
          let
            constraints :: TxConstraints Unit Unit
            constraints =
              mconcat
                [ mustPayToScript valHash assetDatum stakeValue
                , mustPayToScript valHash entryDatum entryValue
                , mustBeSignedBy userPkh
                , mustSpendScriptOutput txIn valRedeemer
                ]

            lookup :: ScriptLookups.ScriptLookups PlutusData
            lookup = mconcat
              [ ScriptLookups.validator validator
              , ScriptLookups.unspentOutputs userUtxos
              , ScriptLookups.unspentOutputs bondedPoolUtxos
              , entryDatumLookup
              ]
          pure $ constraints /\ lookup
        GT -> do
          -- The hashed key is greater than so we must look at the assoc. list
          -- in more detail
          logInfo' "userStakeBondedPoolContract: Compare GT"
          mintingAction
            /\ { firstInput, secondInput }
            /\ { firstOutput, secondOutput }
            /\ { secondKey } <-
            liftContractM
              "userStakeBondedPoolContract: Cannot get position in Assoc. List"
              $ findInsertUpdateElem assocList hashedUserPkh
          let
            valRedeemer = Redeemer $ toData $ StakeAct
              { stakeAmount: amt
              , stakeHolder: userPkh
              , mintingAction
              }

          -- Get the Entry datum of the old assoc. list (first) element
          dHash <-
            liftContractM
              "userStakeBondedPoolContract: Could not get Entry Datum Hash"
              $ getUtxoDatumHash firstOutput
          logInfo_ "userStakeBondedPoolContract: " dHash
          firstListDatum <-
            liftedM
              "userStakeBondedPoolContract: Cannot get \
              \Entry's  datum" $ getDatumByHash dHash
          firstBondedListDatum :: BondedStakingDatum <-
            liftContractM
              "userStakeBondedPoolContract: Cannot extract Assoc. List datum"
              $ fromData (unwrap firstListDatum)

          -- Constraints for the first element.
          firstConstraints /\ firstLookups <- case firstBondedListDatum of
            EntryDatum { entry } -> do
              let e = unwrap entry
              firstEntryDatum <-
                if isJust mintingAction then
                  -- MintInBetween and MintEnd are the same here
                  -- a new middle entry is created so update next
                  pure $ Datum $ toData $ EntryDatum
                    { entry: Entry $ e
                        { next = Just hashedUserPkh
                        }
                    }
                else do -- depositing/updating at the first entry
                  let updateDeposited = e.deposited + amtBigInt
                  unless
                    ( toBigInt minStake <= updateDeposited
                        && updateDeposited
                        <= toBigInt maxStake
                    )
                    $ throwContractError
                        "userStakeBondedPoolContract: Stake amount outside of \
                        \min/max range"
                  pure $ Datum $ toData $ EntryDatum
                    { entry: Entry $ e
                        { newDeposit = e.newDeposit + amtBigInt
                        , deposited = updateDeposited
                        }
                    }
              firstEntryDatumLookup <-
                liftContractM
                  "userStakeBondedPoolContract: Could not create state datum \
                  \lookup"
                  $ ScriptLookups.datum firstEntryDatum
              let
                constr = mconcat
                  [ mustPayToScript valHash firstEntryDatum entryValue
                  , mustSpendScriptOutput firstInput valRedeemer
                  ]
                -- We add validator at the end. If we are minting i.e. when
                -- mintingAction is "Just", we include those in
                -- `middleConstraints` and `middleLookups` below
                lu = firstEntryDatumLookup
              pure $ constr /\ lu
            _ -> throwContractError
              "userStakeBondedPoolContract: Datum not \
              \Entry constructor"

          -- Constraints for the potential middle element.
          middleConstraints /\ middleLookups <-
            if isJust mintingAction then do -- a genuine new entry
              unless (minStake <= amt && amt <= maxStake)
                $ throwContractError
                    "userStakeBondedPoolContract: Stake amount outside of \
                    \min/max range"
              -- Inbetween mint - this should not fail because we have `Just`
              ma <- liftContractM
                "userStakeBondedPoolContract: Could not get \
                \minting action"
                mintingAction
              let
                -- Minting a new Entry
                mintRedeemer = Redeemer $ toData $ ListInsert ma

                entryDatum = Datum $ toData $ EntryDatum
                  { entry: Entry
                      { key: hashedUserPkh
                      , newDeposit: amtBigInt
                      , deposited: amtBigInt
                      , staked: zero
                      , rewards: zero
                      , next: secondKey -- points to original second key
                      }
                  }

              entryDatumLookup <-
                liftContractM
                  "userStakeBondedPoolContract: Could not create state datum \
                  \lookup"
                  $ ScriptLookups.datum entryDatum
              let
                constr = mconcat
                  [ mustMintValueWithRedeemer mintRedeemer entryValue
                  , mustPayToScript valHash entryDatum entryValue
                  ]
                lu = mconcat
                  [ ScriptLookups.mintingPolicy listPolicy
                  , entryDatumLookup
                  ]
              pure $ constr /\ lu
            else pure $ mempty /\ mempty

          -- Get the constraints for the second assoc. list element
          lastConstraints /\ lastLookups <- case secondOutput, secondInput of
            Nothing, Nothing -> pure $ mempty /\ mempty
            Just so, Just si -> do --
              dh <-
                liftContractM
                  "userStakeBondedPoolContract: Could not get Entry Datum Hash"
                  $ getUtxoDatumHash so
              logInfo_ "userStakeBondedPoolContract: " dh
              secondListDatum <-
                liftedM
                  "userStakeBondedPoolContract: Cannot \
                  \get Entry's datum" $ getDatumByHash dh
              secondBondedListDatum :: BondedStakingDatum <-
                liftContractM
                  "userStakeBondedPoolContract: Cannot extract NFT State datum"
                  $ fromData (unwrap secondListDatum)

              -- Unchanged in the case
              lastEntryDatum <- case secondBondedListDatum of
                EntryDatum { entry } ->
                  pure $ Datum $ toData $ EntryDatum { entry }
                _ -> throwContractError
                  "userStakeBondedPoolContract: Datum not \
                  \Entry constructor"

              lastEntryDatumLookup <-
                liftContractM
                  "userStakeBondedPoolContract: Could not create state datum \
                  \lookup"
                  $ ScriptLookups.datum lastEntryDatum
              let
                constr = mconcat
                  [ mustPayToScript valHash lastEntryDatum entryValue
                  , mustSpendScriptOutput si valRedeemer
                  ]
                lu = mconcat
                  [ ScriptLookups.mintingPolicy listPolicy
                  , lastEntryDatumLookup
                  ]
              pure $ constr /\ lu
            _, _ -> throwContractError
              "userStakeBondedPoolContract: Datum not\
              \Entry constructor"
          pure
            $ mconcat
                [ firstConstraints
                , middleConstraints
                , lastConstraints
                , mustBeSignedBy userPkh
                , mustValidateIn range
                ]
            /\
              mconcat
                [ ScriptLookups.validator validator
                , ScriptLookups.unspentOutputs userUtxos
                , ScriptLookups.unspentOutputs bondedPoolUtxos
                , firstLookups
                , middleLookups
                , lastLookups
                ]
    _ -> throwContractError
      "userStakeBondedPoolContract: \
      \Datum incorrect type"

  unattachedBalancedTx <-
    liftedE $ ScriptLookups.mkUnbalancedTx lookup constraints
  logAesonInfo unattachedBalancedTx
  signedTx <-
    liftedM
      "userStakeBondedPoolContract: Cannot balance, reindex redeemers, attach \
      \datums redeemers and sign"
      $ balanceAndSignTx unattachedBalancedTx
  pure { signedTx }
