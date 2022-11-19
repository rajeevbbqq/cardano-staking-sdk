module UnbondedStaking.Utils
  ( calculateRewards
  , getAdminTime
  , getBondingTime
  , getUserTime
  , mkUnbondedPoolParams
  ) where

import Contract.Prelude hiding (length)

import Contract.Address (PaymentPubKeyHash)
import Contract.Monad (Contract, liftContractM, throwContractError)
import Contract.Numeric.Rational (Rational, (%))
import Contract.Value (CurrencySymbol)
import Data.Array (filter, head, takeWhile, (..))
import Data.BigInt (BigInt, quot, toInt)
import Types.Interval (POSIXTime(POSIXTime), POSIXTimeRange, interval)
import UnbondedStaking.Types
  ( UnbondedPoolParams(UnbondedPoolParams)
  , InitialUnbondedParams(InitialUnbondedParams)
  )
import Utils (big, currentRoundedTime, mkRatUnsafe)

-- | Admin deposit/closing
getAdminTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getAdminTime (UnbondedPoolParams upp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

    adminStart :: BigInt
    adminStart = upp.start + upp.userLength

    adminEnd :: BigInt
    adminEnd = adminStart + upp.adminLength - big 1000
  -- Return range
  start /\ end <- liftContractM "getAdminTime: this is not a admin period" $
    isWithinPeriod currTime' cycleLength adminStart adminEnd
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | User deposits/withdrawals
getUserTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getUserTime (UnbondedPoolParams upp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

    userStart :: BigInt
    userStart = upp.start

    userEnd :: BigInt
    userEnd = userStart + upp.userLength - big 1000
  -- Return range
  start /\ end <- liftContractM "getUserTime: this is not a user period" $
    isWithinPeriod currTime' cycleLength userStart userEnd
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | User withdrawals only
-- | Note: Period can either be in bonding period or user period
getBondingTime
  :: forall (r :: Row Type)
   . UnbondedPoolParams
  -> Contract r { currTime :: POSIXTime, range :: POSIXTimeRange }
getBondingTime (UnbondedPoolParams upp) = do
  -- Get time and round it up to the nearest second
  currTime@(POSIXTime currTime') <- currentRoundedTime
  -- Get timerange in which the staking should be done
  let
    cycleLength :: BigInt
    cycleLength = upp.userLength + upp.adminLength + upp.bondingLength

    userStart :: BigInt
    userStart = upp.start

    userEnd :: BigInt
    userEnd = userStart + upp.userLength - big 1000

    bondingStart :: BigInt
    bondingStart = upp.start + upp.userLength + upp.adminLength

    bondingEnd :: BigInt
    bondingEnd = bondingStart + upp.bondingLength - big 1000
    -- Period ranges
    userPeriod = isWithinPeriod currTime' cycleLength userStart userEnd
    bondingPeriod = isWithinPeriod currTime' cycleLength bondingStart bondingEnd

    getPeriod
      :: Maybe (Tuple BigInt BigInt)
      -> Maybe (Tuple BigInt BigInt)
      -> Maybe (Tuple BigInt BigInt)
    getPeriod user bonding = case user /\ bonding of
      user' /\ Nothing -> user'
      Nothing /\ bonding' -> bonding'
      _ /\ _ -> Nothing
  -- Return range
  start /\ end <- liftContractM "getUserTime: this is not a user/bonding period"
    $
      getPeriod userPeriod bondingPeriod
  pure { currTime, range: interval (POSIXTime start) (POSIXTime end) }

-- | Returns the current/previous period and the next valid period in the
-- | future from the current time
isWithinPeriod
  :: forall (r :: Row Type)
   . BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Maybe (Tuple BigInt BigInt)
isWithinPeriod currTime cycleLength start end =
  let
    currentIteration :: Int
    currentIteration = fromMaybe 0 $ toInt (start `quot` cycleLength)

    upperBound :: Int
    upperBound = 1 + currentIteration

    possibleRanges :: Array (Tuple BigInt BigInt)
    possibleRanges =
      ( \i -> (cycleLength * big (i - 1) + start)
          /\ (cycleLength * big (i - 1) + end)
      )
        <$> 1 .. upperBound

    periods :: Array (Tuple BigInt BigInt)
    periods =
      takeWhile (\(start' /\ _) -> currTime >= start')
        possibleRanges

    currentPeriod :: Array (Tuple BigInt BigInt)
    currentPeriod =
      filter
        (\(start' /\ end') -> start' <= currTime && currTime < end')
        periods
  in
    if null currentPeriod then Nothing else head currentPeriod

-- | Creates the `UnbondedPoolParams` from the `InitialUnbondedParams` and
-- | runtime parameters from the user.
mkUnbondedPoolParams
  :: PaymentPubKeyHash
  -> CurrencySymbol
  -> CurrencySymbol
  -> InitialUnbondedParams
  -> UnbondedPoolParams
mkUnbondedPoolParams admin nftCs assocListCs (InitialUnbondedParams iup) = do
  UnbondedPoolParams
    { start: iup.start
    , userLength: iup.userLength
    , adminLength: iup.adminLength
    , bondingLength: iup.bondingLength
    , interestLength: iup.interestLength
    , increments: iup.increments
    , interest: iup.interest
    , minStake: iup.minStake
    , maxStake: iup.maxStake
    , admin
    , unbondedAssetClass: iup.unbondedAssetClass
    , nftCs
    , assocListCs
    }

-- | Calculates user awards according to spec formula
calculateRewards
  :: Rational
  -> BigInt
  -> BigInt
  -> BigInt
  -> BigInt
  -> Contract () Rational
calculateRewards rewards totalRewards deposited newDeposit totalDeposited = do
  -- New users will have zero total deposited for the first cycle
  if totalDeposited == zero then
    pure zero
  else do
    let
      lhs = mkRatUnsafe $ totalRewards % totalDeposited
      rhs = rewards + mkRatUnsafe (deposited % one)
      rhs' = rhs - mkRatUnsafe (newDeposit % one)
      f = rhs' * lhs
    when (f < zero) $ throwContractError
      "calculateRewards: invalid rewards amount"
    pure $ rewards + f
