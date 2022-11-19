module BondedStaking.TimeUtils
  ( getBondingTime
  , getStakingTime
  , getWithdrawingTime
  , getClosingTime
  , startPoolFromNow
  , startPoolNow
  , toSlotInterval
  , fromSlotInterval
  ) where

import Contract.Prelude

import Contract.Monad
  ( Contract
  , liftContractE
  , liftContractM
  , throwContractError
  )
import Contract.Numeric.Natural (toBigInt)
import Contract.Time (Slot, getEraSummaries, getSystemStart, slotToPosixTime)
import Control.Alternative (guard)
import Data.Array (head)
import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Types
  ( BondedPoolParams(BondedPoolParams)
  , InitialBondedParams(InitialBondedParams)
  )
import Types.Interval
  ( POSIXTime(POSIXTime)
  , POSIXTimeRange
  , interval
  , posixTimeRangeToTransactionValidity
  )
import Types.Natural (Natural)
import Utils (big, bigIntRange, currentRoundedTime)

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as a valid staking period. If there is no such range,
-- | throw an error.
getStakingTime
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getStakingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if staking is impossible
  when (currTime' > bpp.end) $
    throwContractError "getStakingTime: pool already closed"
  when (currTime' > bpp.end - bpp.userLength) $
    throwContractError "getStakingTime: pool in withdrawing only period"
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = bpp.bondingLength + bpp.userLength

    possibleRanges :: Array (BigInt /\ BigInt)
    possibleRanges = do
      -- Range from 0 to bpp.iterations
      n <- bigIntRange $ toBigInt bpp.iterations
      -- Calculate start and end of the range
      let
        range@(_start /\ end) = (bpp.start + n * cycleLength) /\
          (bpp.start + n * cycleLength + bpp.userLength - big 1000)
      -- Discard range if end < currTime
      guard $ currTime' <= end
      -- NOTE: We don't discard these ranges yet because it's easier to debug when CTL submits
      -- the TX and fails loudly
      ---- Discard range if currTime < start
      --guard $ currTime' >= start
      pure range
  -- Return first range
  start /\ end <- liftContractM "getStakingTime: this is not a staking period" $
    head possibleRanges
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as a valid deposit period. If there is no such range,
-- | throw an error.
getBondingTime
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getBondingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if depositing is impossible
  when (currTime' > bpp.end) $
    throwContractError "getBondingTime: pool already closed"
  when (currTime' > bpp.end - bpp.userLength) $
    throwContractError "getBondingTime: pool in withdrawing only period"
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = bpp.bondingLength + bpp.userLength

    possibleRanges :: Array (BigInt /\ BigInt)
    possibleRanges = do
      -- Range from 0 to bpp.iterations
      n <- bigIntRange $ toBigInt bpp.iterations
      -- Calculate start and end of the range
      let
        range@(_start /\ end) = (bpp.start + n * cycleLength + bpp.userLength)
          /\ (bpp.start + (n + one) * cycleLength - big 1000)
      -- Discard range if end < currTime
      guard $ currTime' <= end
      -- Discard range if currTime < start
      -- guard $ currTime' >= start
      pure range
  -- Return first range
  start /\ end <- liftContractM "getBondingTime: this is not a bonding period" $
    head possibleRanges
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as a withdrawing period. These periods are the same
-- | as the ones returned by `getStakingTime`, with the addition of a final
-- | withdrawing-only period before pool closure. If there is no such range,
-- | throw an error.
getWithdrawingTime
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getWithdrawingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if withdrawing is impossible
  when (currTime' > bpp.end) $
    throwContractError "getWithdrawingTime: pool already closed"
  -- If currTime is inside withdrawing-only period, return it. Otherwise,
  -- check all the staking periods
  if (currTime' > bpp.end - bpp.userLength) then pure
    { currTime
    , range: interval (POSIXTime $ bpp.end - bpp.userLength)
        (POSIXTime $ bpp.end - big 1000)
    }
  else do
    -- Get timerange in which the staking should be done
    let
      cycleLength :: BigInt
      cycleLength = bpp.bondingLength + bpp.userLength

      possibleRanges :: Array (BigInt /\ BigInt)
      possibleRanges = do
        -- Range from 0 to bpp.iterations
        n <- bigIntRange $ toBigInt bpp.iterations
        -- Calculate start and end of the range (same as staking range)
        let
          range@(_start /\ end) = (bpp.start + n * cycleLength) /\
            (bpp.start + n * cycleLength + bpp.userLength - big 1000)
        -- Discard range if end < currTime
        guard $ currTime' <= end
        -- Discard range if currTime < start
        -- guard $ currTime' >= start
        pure range
    -- Return first range
    start /\ end <- liftContractM "getBondingTime: this is not a bonding period"
      $
        head possibleRanges
    pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | Get the time-range that includes the current (approximate) time and the
-- | pool accepts as the valid closing period. If there is no such range,
-- | throw an error.
getClosingTime
  :: forall (r :: Row Type)
   . BondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getClosingTime (BondedPoolParams bpp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Throw error if the pool can't close yet
  when (currTime' < bpp.end) $
    throwContractError "getClosingTime: pool can't close yet"
  -- Otherwise, return range from now to 1 hour from now
  -- NOTE: It's an error to use an open range (i.e. to infinity). The validator
  -- will fail if it detects that.
  pure
    { currTime
    , range: interval (POSIXTime bpp.end)
        (POSIXTime $ bpp.end + BigInt.fromInt 3_600_000)
    }

-- | Substitute the `start` and `end` field of the initial pool parameters to
-- | make the pool start `delay` seconds from the current time. Return the
-- | updated pool parameters and the current time approximate time.
-- NOTE: We should add another intermediate type between `InitialBondedParams`
-- and `BondedPoolParams` that reflects the addition of the pool start and
-- end times
startPoolFromNow
  :: forall (r :: Row Type)
   . Natural
  -> InitialBondedParams
  -> Contract r (InitialBondedParams /\ POSIXTime)
startPoolFromNow delay (InitialBondedParams ibp) = do
  POSIXTime currTime <- currentRoundedTime
  let
    ibp' = InitialBondedParams $ ibp
      { start = currTime + toBigInt delay
      , end = currTime + toBigInt delay + toBigInt ibp.iterations
          * (ibp.userLength + ibp.bondingLength)
          + ibp.userLength
      }
  pure $ ibp' /\ POSIXTime currTime

-- | Substitute the `start` and `end` field of the initial pool parameters to
-- | make the pool start immediately (as specified by the value of
-- | `currentRoundedTime`). Return the updated pool parameters and the
-- | current time approximate time.
startPoolNow
  :: forall (r :: Row Type)
   . InitialBondedParams
  -> Contract r (InitialBondedParams /\ POSIXTime)
startPoolNow (InitialBondedParams ibp) = do
  POSIXTime currTime <- currentRoundedTime
  let
    ibp' = InitialBondedParams $ ibp
      { start = currTime
      , end = currTime + toBigInt ibp.iterations
          * (ibp.userLength + ibp.bondingLength)
          + ibp.userLength
      }
  pure $ ibp' /\ POSIXTime currTime

-- | Convert a `POSIXTimeRange` to a `Slot` interval
toSlotInterval
  :: forall (r :: Row Type)
   . POSIXTimeRange
  -> Contract r
       { validityStartInterval :: Maybe Slot, timeToLive :: Maybe Slot }
toSlotInterval posixTimeRange = do
  es <- getEraSummaries
  ss <- getSystemStart
  liftContractE <=< liftEffect $ posixTimeRangeToTransactionValidity es ss
    posixTimeRange

-- | Convert a `Slot` to a `POSIXTimeRange` interval
fromSlotInterval
  :: forall (r :: Row Type)
   . { validityStartInterval :: Maybe Slot, timeToLive :: Maybe Slot }
  -> Contract r { from :: POSIXTime, to :: POSIXTime }
fromSlotInterval r = do
  es <- getEraSummaries
  ss <- getSystemStart
  startSlot <- liftContractM
    "fromSlotInterval: could not get validityStartInterval slot"
    r.validityStartInterval
  endSlot <- liftContractM "fromSlotInterval: could not get timeToLive slot"
    r.timeToLive
  from <- liftContractE =<< (liftEffect $ slotToPosixTime es ss startSlot)
  to <- liftContractE =<< (liftEffect $ slotToPosixTime es ss endSlot)
  pure { from, to }

