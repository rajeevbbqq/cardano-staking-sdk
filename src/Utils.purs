module Utils
  ( big
  , bigIntRange
  , currentRoundedTime
  , currentTime
  , countdownTo
  , findInsertUpdateElem
  , findRemoveOtherElem
  , getAssetsToConsume
  , getUtxoWithNFT
  , hashPkh
  , jsonReader
  , logInfo_
  , mkAssetUtxosConstraints
  , mkBondedPoolParams
  , mkOnchainAssocList
  , mkRatUnsafe
  , nat
  , roundDown
  , roundUp
  , splitByLength
  , submitTransaction
  , toIntUnsafe
  , repeatUntilConfirmed
  , mustPayToScript
  , getUtxoDatumHash
  , addressFromBech32
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (Address, Bech32String, PaymentPubKeyHash, getNetworkId)
import Contract.Hashing (blake2b256Hash)
import Contract.Log (logInfo, logInfo', logAesonInfo)
import Contract.Monad
  ( Contract
  , liftContractM
  , liftedE
  , liftedM
  , tag
  , throwContractError
  )
import Contract.Numeric.Natural (Natural, fromBigInt', toBigInt)
import Contract.Numeric.Rational (Rational, numerator, denominator)
import Contract.PlutusData (Datum, DataHash, PlutusData)
import Contract.Prim.ByteArray (ByteArray, hexToByteArray)
import Contract.ScriptLookups as ScriptLookups
import Contract.Scripts (PlutusScript)
import Contract.Scripts (ValidatorHash)
import Contract.Time
  ( ChainTip(..)
  , Tip(..)
  , getEraSummaries
  , getSystemStart
  , getTip
  , slotToPosixTime
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , BalancedSignedTransaction
  , balanceAndSignTx
  , submit
  , awaitTxConfirmedWithTimeout
  , plutusV1Script
  )
import Contract.TxConstraints
  ( TxConstraints
  , DatumPresence(DatumWitness)
  , mustSpendScriptOutput
  , mustPayToScript
  )
import Contract.TxConstraints as TxConstraints
import Contract.Time
  ( ChainTip(..)
  , Tip(..)
  , getEraSummaries
  , getSystemStart
  , getTip
  , slotToPosixTime
  )
import Contract.Transaction
  ( TransactionInput
  , TransactionOutputWithRefScript(TransactionOutputWithRefScript)
  , BalancedSignedTransaction
  , balanceAndSignTx
  , submit
  , awaitTxConfirmedWithTimeout
  , plutusV1Script
  , TransactionHash
  )
import Contract.TxConstraints
  ( TxConstraints
  , DatumPresence(DatumWitness)
  , mustSpendScriptOutput
  , mustPayToScript
  )
import Contract.TxConstraints as TxConstraints
import Contract.PlutusData (Datum, DataHash, PlutusData)
import Contract.Scripts (ValidatorHash, PlutusScript)
import Contract.Utxos (UtxoMap)
import Contract.Value
  ( CurrencySymbol
  , TokenName
  , Value
  , flattenNonAdaAssets
  , getTokenName
  , valueOf
  )
import Control.Alternative (guard)
import Control.Monad.Error.Class (liftMaybe, throwError, try)
import Data.Argonaut.Core (Json, caseJsonObject)
import Data.Argonaut.Decode.Combinators (getField) as Json
import Data.Argonaut.Decode.Error (JsonDecodeError(TypeMismatch))
import Data.Array
  ( filter
  , head
  , last
  , length
  , partition
  , mapMaybe
  , slice
  , sortBy
  , (..)
  )
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt, fromNumber, quot, rem, toInt, toNumber)
import Prim.Row (class Lacks)
import Record (insert, delete)
import Data.Map (Map, toUnfoldable)
import Data.Map as Map
import Data.Symbol (SProxy(..))
import Data.Time.Duration (Seconds, Milliseconds(Milliseconds))
import Data.Unfoldable (unfoldr)
import Effect.Aff (delay)
import Effect.Exception (error, throw)
import Math (ceil)
import Plutus.Conversion (toPlutusAddress)
import Serialization.Address (addressFromBech32, addressNetworkId) as SA
import Serialization.Hash (ed25519KeyHashToBytes)
import Types
  ( AssetClass(AssetClass)
  , BondedPoolParams(BondedPoolParams)
  , InitialBondedParams(InitialBondedParams)
  , MintingAction(MintEnd, MintInBetween)
  )
import Types.Interval (POSIXTime(POSIXTime))
import Types.OutputDatum (OutputDatum(OutputDatumHash))
import Types.ByteArray (byteArrayToHex)
import Types.Redeemer (Redeemer)

-- | Helper to decode the local inputs such as unapplied minting policy and
-- typed validator
jsonReader
  :: String
  -> Json
  -> Either JsonDecodeError PlutusScript
jsonReader field = do
  caseJsonObject (Left $ TypeMismatch "Expected Object") $ \o -> do
    hex <- Json.getField o field
    case hexToByteArray hex of
      Nothing -> Left $ TypeMismatch "Could not convert to bytes"
      Just bytes -> pure $ plutusV1Script bytes

-- | Get the UTXO with the NFT defined by its `CurrencySymbol` and `TokenName`.
-- If more than one UTXO contains the NFT, something is seriously wrong.
getUtxoWithNFT
  :: UtxoMap
  -> CurrencySymbol
  -> TokenName
  -> Maybe (Tuple TransactionInput TransactionOutputWithRefScript)
getUtxoWithNFT utxoM cs tn =
  let
    utxos = filter hasNFT $ toUnfoldable utxoM
  in
    if length utxos > 1 then Nothing
    else head utxos
  where
  hasNFT
    :: Tuple TransactionInput TransactionOutputWithRefScript
    -> Boolean
  hasNFT (Tuple _ txOutput') =
    let
      txOutput = unwrap $ (unwrap txOutput').output
    in
      valueOf txOutput.amount cs tn == one

-- | This receives a `UtxoMap` with all the asset UTxOs of the pool and the desired
-- | amount to withdraw. It returns a subset of these that sums at least
-- | the given amount and the total amount
getAssetsToConsume
  :: AssetClass -> BigInt -> UtxoMap -> Maybe (UtxoMap /\ BigInt)
getAssetsToConsume (AssetClass ac) withdrawAmt assetUtxos =
  go assetList Map.empty zero
  where
  assetList :: Array (TransactionInput /\ TransactionOutputWithRefScript)
  assetList = Map.toUnfoldable $ assetUtxos

  go
    :: Array (TransactionInput /\ TransactionOutputWithRefScript)
    -> Map TransactionInput TransactionOutputWithRefScript
    -> BigInt
    -> Maybe (UtxoMap /\ BigInt)
  go arr toConsume sum
    | sum >= withdrawAmt = Just $ toConsume /\ (sum - withdrawAmt)
    | null arr = Nothing
    | otherwise = do
        input /\ output <- Array.head arr
        arr' <- Array.tail arr
        let
          assetCount = valueOf (unwrap (unwrap output).output).amount
            ac.currencySymbol
            ac.tokenName
          toConsume' = Map.insert input output toConsume
          sum' = sum + assetCount
        go arr' toConsume' sum'

-- | Builds constraints for asset UTxOs
mkAssetUtxosConstraints :: UtxoMap -> Redeemer -> TxConstraints Unit Unit
mkAssetUtxosConstraints utxos redeemer =
  foldMap (\(input /\ _) -> mustSpendScriptOutput input redeemer)
    ( Map.toUnfoldable $ utxos
        :: Array (TransactionInput /\ TransactionOutputWithRefScript)
    )

-- | Convert from `Int` to `Natural`
nat :: Int -> Natural
nat = fromBigInt' <<< fromInt

-- | Convert from `Int` to `BigInt`
big :: Int -> BigInt
big = fromInt

roundUp :: Rational -> BigInt
roundUp r =
  let
    n = numerator r
    d = denominator r
  in
    if d == one then n
    else quot (n + d - (rem n d)) d

roundDown :: Rational -> BigInt
roundDown r =
  let
    n = numerator r
    d = denominator r
  in
    quot (n - (rem n d)) d

-- | Converts a `Maybe Rational` to a `Rational` when using the (%) constructor
mkRatUnsafe :: Maybe Rational -> Rational
mkRatUnsafe Nothing = zero
mkRatUnsafe (Just r) = r

-- | Converts from a contract 'Natural' to an 'Int'
toIntUnsafe :: Natural -> Int
toIntUnsafe = fromMaybe 0 <<< toInt <<< toBigInt

logInfo_
  :: forall (r :: Row Type) (a :: Type)
   . Show a
  => String
  -> a
  -> Contract r Unit
logInfo_ k = flip logInfo mempty <<< tag k <<< show

-- Creates the `BondedPoolParams` from the `InitialBondedParams` and runtime
-- parameters from the user.
mkBondedPoolParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> InitialBondedParams
  -> BondedPoolParams
mkBondedPoolParams admin nftCs assocListCs (InitialBondedParams ibp) = do
  BondedPoolParams
    { iterations: ibp.iterations
    , start: ibp.start
    , end: ibp.end
    , userLength: ibp.userLength
    , bondingLength: ibp.bondingLength
    , interest: ibp.interest
    , minStake: ibp.minStake
    , maxStake: ibp.maxStake
    , admin
    , bondedAssetClass: ibp.bondedAssetClass
    , nftCs
    , assocListCs
    }

hashPkh :: PaymentPubKeyHash -> Aff ByteArray
hashPkh =
  pure <<< blake2b256Hash <<< unwrap <<< ed25519KeyHashToBytes <<< unwrap <<<
    unwrap

-- | Makes an on chain assoc list returning the key, input and output. We could
-- | be more stringent on checks to ensure the list is genuinely connected
-- | although on chain code should enforce this.
mkOnchainAssocList
  :: CurrencySymbol
  -> UtxoMap
  -> Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
mkOnchainAssocList assocListCs utxos =
  sortBy compareBytes $ mapMaybe getAssocListUtxos $ toUnfoldable utxos
  where
  getAssocListUtxos
    :: TransactionInput /\ TransactionOutputWithRefScript
    -> Maybe (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  getAssocListUtxos utxo@(_ /\ (TransactionOutputWithRefScript txOutput)) = do
    let val = flattenNonAdaAssets (unwrap (txOutput.output)).amount
    cs /\ tn /\ amt <- head val
    guard (length val == one && cs == assocListCs && amt == one)
    pure $ getTokenName tn /\ utxo

compareBytes
  :: forall (t :: Type). ByteArray /\ t -> ByteArray /\ t -> Ordering
compareBytes (bytes /\ _) (bytes' /\ _) = compare bytes bytes'

-- | Find the assoc list element to update or insert. This can be optimised
-- | if we compare pairs and exit early of course. But we'll do this for
-- | simplicity. THIS MUST BE USED ON A SORTED LIST, i.e. with
-- | `mkOnchainAssocList`. We should probably create a type for the output.
findInsertUpdateElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> ByteArray
  -> Maybe
       ( Maybe MintingAction
           /\
             { firstInput :: TransactionInput
             , secondInput :: Maybe TransactionInput
             }
           /\
             { firstOutput :: TransactionOutputWithRefScript
             , secondOutput :: Maybe TransactionOutputWithRefScript
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: Maybe ByteArray
             }
       )
findInsertUpdateElem assocList hashedKey = do
  -- The list should findAssocElem assocList hashedKey = do be sorted so no
  -- need to resort
  let { no, yes } = partition (fst >>> (>=) hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  -- If we're at the last element, it must be an end stake or updating last
  -- element
  if length no == zero then do
    -- Workout whether it's an initial deposit
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintEnd txInputL
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Nothing }
      /\ { firstOutput: txOutputL, secondOutput: Nothing }
      /\ { firstKey: bytesL, secondKey: Nothing }
  -- Otherwise, it is an inbetween stake or updating the first element
  else do
    bytesH /\ txInputH /\ txOutputH <- head no
    let
      mintingAction =
        if bytesL == hashedKey then Nothing
        else Just $ MintInBetween txInputL txInputH
    pure
      $ mintingAction
      /\ { firstInput: txInputL, secondInput: Just txInputH }
      /\ { firstOutput: txOutputL, secondOutput: Just txOutputH }
      /\ { firstKey: bytesL, secondKey: Just bytesH }

-- | Find the element to remove from the list. This only works for the
-- | in-between case, since it assumes that some entry will have a key less
-- | than the given one.
findRemoveOtherElem
  :: Array (ByteArray /\ TransactionInput /\ TransactionOutputWithRefScript)
  -> ByteArray
  -> Maybe
       ( { firstInput :: TransactionInput
         , secondInput :: TransactionInput
         }
           /\
             { firstOutput :: TransactionOutputWithRefScript
             , secondOutput :: TransactionOutputWithRefScript
             }
           /\
             { firstKey :: ByteArray
             , secondKey :: ByteArray
             }
       )
findRemoveOtherElem assocList hashedKey = do
  let { no, yes } = partition (fst >>> (<) hashedKey) assocList
  bytesL /\ txInputL /\ txOutputL <- last yes
  bytesH /\ txInputH /\ txOutputH <- head no
  if bytesH /= hashedKey
  -- If the first element not less than `hashedKey` is not equal, then the
  -- entry has not been found
  then Nothing
  -- Otherwise, this is the entry to remove and the last element of the
  -- entries less than `hashedKey` is the previous entry
  else Just
    $ { firstInput: txInputL, secondInput: txInputH }
    /\ { firstOutput: txOutputL, secondOutput: txOutputH }
    /\ { firstKey: bytesL, secondKey: bytesH }

-- Produce a range from zero to the given bigInt (inclusive)
bigIntRange :: BigInt -> Array BigInt
bigIntRange lim =
  unfoldr
    ( \acc ->
        if acc >= lim then Nothing
        else Just $ acc /\ (acc + one)
    )
    zero

-- Get the node's time rounded to the closest integer (ceiling) in seconds.
currentRoundedTime
  :: forall (r :: Row Type). Contract r POSIXTime
currentRoundedTime = do
  POSIXTime t <- currentTime
  t' <-
    liftMaybe (error "currentRoundedTime: could not convert Number to BigInt")
      $ fromNumber
      $ ceil (toNumber t / 1000.0)
      * 1000.0
  pure $ POSIXTime t'

-- | Get the POSIX time from the node. This is obtained by converting the current
-- | slot.
currentTime
  :: forall (r :: Row Type). Contract r POSIXTime
currentTime = do
  -- Get current slot
  ChainTip { slot } <- getTip >>= case _ of
    Tip chainTip -> pure chainTip
    TipAtGenesis -> throwContractError "currentTime: node returned TipAtGenesis"
  -- Convert slot to POSIXTime
  es <- getEraSummaries
  ss <- getSystemStart
  liftEither
    <<< lmap (error <<< show)
    <=< liftEffect
    $ slotToPosixTime es ss slot

countdownTo :: forall (r :: Row Type). POSIXTime -> Contract r Unit
countdownTo targetTime = countdownTo' Nothing
  where
  countdownTo' :: Maybe POSIXTime -> Contract r Unit
  countdownTo' prevTime = do
    currTime <- currentTime
    let
      msg :: String
      msg = "Countdown: " <> showSeconds delta

      delta :: BigInt
      delta = unwrap targetTime - unwrap currTime
    if currTime >= targetTime then logInfo' "GO"
    else if timeChanged prevTime currTime then logInfo' msg *> wait delta *>
      countdownTo' (Just currTime)
    else wait delta *> countdownTo' (Just currTime)

  wait :: BigInt -> Contract r Unit
  wait n = liftAff $ delay $
    if n > fromInt 5000 then Milliseconds 5000.0
    else Milliseconds $ toNumber n

  showSeconds :: BigInt -> String
  showSeconds n = show $ toNumber n / 1000.0

  timeChanged :: Maybe POSIXTime -> POSIXTime -> Boolean
  timeChanged prevTime currTime = maybe true (_ /= currTime) prevTime

-- | Utility function for splitting an array into equal length sub-arrays
-- | (with remainder array length <= size)
splitByLength :: forall (a :: Type). Int -> Array a -> Array (Array a)
splitByLength size array
  | size == 0 || null array = []
  | otherwise =
      let
        sublistCount =
          if (length array) `mod` size == 0 then ((length array) `div` size) - 1
          else (length array) `div` size
      in
        map (\i -> slice (i * size) ((i * size) + size) array) $
          0 .. sublistCount

-- | Submits a transaction with the given list of constraints/lookups
submitTransaction
  :: TxConstraints Unit Unit
  -> ScriptLookups.ScriptLookups PlutusData
  -> Array
       ( Tuple
           (TxConstraints Unit Unit)
           (ScriptLookups.ScriptLookups PlutusData)
       )
  -> Seconds
  -> Int
  -> Contract ()
       ( Array
           ( Tuple
               (TxConstraints Unit Unit)
               (ScriptLookups.ScriptLookups PlutusData)
           )
       )
submitTransaction baseConstraints baseLookups updateList timeout maxAttempts =
  do
    let
      constraintList = fst <$> updateList
      lookupList = snd <$> updateList
      constraints = baseConstraints <> mconcat constraintList
      lookups = baseLookups <> mconcat lookupList
    result <- try $ repeatUntilConfirmed timeout maxAttempts do
      -- Build transaction
      unattachedBalancedTx <-
        liftedE $ ScriptLookups.mkUnbalancedTx lookups constraints
      logAesonInfo unattachedBalancedTx
      signedTx <-
        liftedM
          "submitTransaction: Cannot balance, reindex redeemers, /\
          \attach datums redeemers and sign"
          $ balanceAndSignTx unattachedBalancedTx
      pure { signedTx }
    case result of
      Left e -> do
        logInfo_ "submitTransaction:" e
        pure updateList
      Right _ ->
        pure []

-- | This function executes a `Contract` that returns a `BalancedSignedTransaction`,
-- | submits it, and waits `timeout` seconds for it to succeed. If it does not,
-- | the function repeats the contract execution and TX submission until it
-- | does or `maxTrials` attempts are completed.
repeatUntilConfirmed
  :: forall (r :: Row Type) (p :: Row Type)
   . Lacks "txId" p
  => Lacks "signedTx" p
  => Seconds
  -> Int
  -> Contract r { signedTx :: BalancedSignedTransaction | p }
  -> Contract r
       { txId :: String | p }
repeatUntilConfirmed timeout maxTrials contract = do
  result@{ signedTx } <- contract
  logInfo' "repeatUntilConfirmed: transaction built successfully"
  txHash <- submit signedTx
  logInfo'
    "repeatUntilConfirmed: transaction submitted. Waiting for confirmation"
  confirmation <- try $ awaitTxConfirmedWithTimeout timeout txHash
  case confirmation of
    Left _ -> do
      logInfo'
        "repeatUntilConfirmed: timeout reached, the transaction was not confirmed"
      if maxTrials == 0 then do
        logInfo'
          "repeatUntilConfirmed: no more trials remaining, throwing exception..."
        liftEffect $ throw "Failed to submit transaction"
      else do
        logInfo' $ "repeatUntilConfirmed: running transaction again, "
          <> show maxTrials
          <> " trials remaining"
        repeatUntilConfirmed timeout (maxTrials - 1) contract
    Right _ -> do
      logInfo' "repeatUntilConfirmed: transaction confirmed!"
      logInfo_ "TX Hash" txHash
      -- pure $ insert { txId: txHash } result
      pure $ insert (SProxy :: SProxy "txId") (byteArrayToHex $ unwrap txHash)
        (delete (SProxy :: SProxy "signedTx") result)

mustPayToScript
  :: forall (i :: Type) (o :: Type)
   . ValidatorHash
  -> Datum
  -> Value
  -> TxConstraints i o
mustPayToScript vh dat = TxConstraints.mustPayToScript vh dat DatumWitness

getUtxoDatumHash :: TransactionOutputWithRefScript -> Maybe DataHash
getUtxoDatumHash = unwrap >>> _.output >>> unwrap >>> _.datum >>> case _ of
  OutputDatumHash dh -> pure dh
  _ -> Nothing

-- Copied from newer CTL revision
addressFromBech32 :: Bech32String -> Contract () Address
addressFromBech32 str = do
  networkId <- getNetworkId
  cslAddress <- liftContractM "addressFromBech32: unable to read address" $
    SA.addressFromBech32 str
  address <-
    liftContractM "addressFromBech32: unable to convert to plutus address" $
      toPlutusAddress cslAddress
  when (networkId /= SA.addressNetworkId cslAddress)
    (throwError $ error "addressFromBech32: address has wrong NetworkId")
  pure address
