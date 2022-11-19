module Main (main) where

import Contract.Prelude

import BondedStaking.TimeUtils (startPoolFromNow)
import ClosePool (closeBondedPoolContract)
import Contract.Address (NetworkId(TestnetId))
import Contract.Config (WalletSpec(..))
import Contract.Log (logInfo')
import Contract.Monad
  ( Contract
  , defaultDatumCacheWsConfig
  , defaultOgmiosWsConfig
  , defaultServerConfig
  , launchAff_
  , liftContractM
  , runContract
  )
import CreatePool (createBondedPoolContract)
import Data.BigInt as BigInt
import Data.Int as Int
import Data.Time.Duration (Milliseconds(..))
import DepositPool (depositBondedPoolContract)
import Effect.Aff (delay)
import Effect.Exception (error)
import Settings (testInitBondedParams)
import Types (BondedPoolParams(..))
import Types.Interval (POSIXTime(..))
import Types.Natural as Natural
import UserStake (userStakeBondedPoolContract)
import UserWithdraw (userWithdrawBondedPoolContract)
import Utils (logInfo_, countdownTo)

-- import Settings (testInitUnbondedParams)
-- import UnbondedStaking.ClosePool (closeUnbondedPoolContract)
-- import UnbondedStaking.CreatePool (createUnbondedPoolContract)
-- import UnbondedStaking.DepositPool (depositUnbondedPoolContract)
-- import UnbondedStaking.Types
--   ( InitialUnbondedParams(InitialUnbondedParams)
--   , UnbondedPoolParams(..)
--   )
-- import UnbondedStaking.UserStake (userStakeUnbondedPoolContract)
-- import UnbondedStaking.UserWithdraw (userWithdrawUnbondedPoolContract)
-- import Utils (currentRoundedTime)
-- import Types.Interval (POSIXTime(POSIXTime))

-- main :: Effect Unit
-- main = launchAff_ $ do
--   cfg <- mkConfig
--   runContract_ cfg $ do
--     initParams <- liftContractM "main: Cannot initiate bonded parameters"
--       testInitBondedParams
--     bondedParams <- createBondedPoolContract initParams
--     -- sleep in order to wait for tx
--     liftAff $ delay $ wrap $ toNumber 80_000
--     depositBondedPoolContract bondedParams
--     liftAff $ delay $ wrap $ toNumber 80_000
--     closeBondedPoolContract bondedParams

main :: Effect Unit
main = testBonded -- write here the desired test

-- | Bonded: admin creates pool, user stakes, admin deposits (rewards),
-- | user withdraws (stakes and rewards), admin closes the pool. Uses
-- | PureScript (not the SDK)
testBonded :: Effect Unit
testBonded = launchAff_ do
  log "STARTING AS ADMIN"
  let
    startDelayInt :: Int
    startDelayInt = 80_000

    waitForWalletChange :: String -> Aff Unit
    waitForWalletChange role = do
      let n = 20
      log $ "SWITCH WALLETS NOW - CHANGE TO BACK TO " <> role
      log $ show n
      waitN n

  ---- Admin creates pool ----
  bondedParams@(BondedPoolParams bpp) <-
    runContract_ do
      -- We get the initial parameters of the pool (no time information, no currency symbols)
      initParams <- liftContractM "main: Cannot initiate bonded parameters"
        testInitBondedParams
      -- We get the current time and set up the pool to start 80 seconds from now
      startDelay <- liftContractM "main: Cannot create startDelay from Int"
        $ Natural.fromBigInt
        $ BigInt.fromInt startDelayInt
      -- We build the initial parameters of the pool, now with time information (no currency symbols)
      initParams' /\ currTime <- startPoolFromNow startDelay initParams
      logInfo_ "Pool creation time" currTime
      -- We build the transaction and submit it. We finally get all the parameters of the pool
      { bondedPoolParams } <- createBondedPoolContract initParams'
      logInfo_ "Pool parameters" bondedPoolParams
      pure bondedPoolParams

  waitForWalletChange "USER 1"
  log "Waiting for pool start..."
  countdownTo' $ POSIXTime bpp.start

  ---- User 1 deposits ----
  userStake <- liftM (error "main: Cannot create userStake from String") $
    Natural.fromString "40000"
  _ <- runContract_ $
    userStakeBondedPoolContract bondedParams userStake

  waitForWalletChange "ADMIN"
  log "Waiting for bonding period..."
  countdownTo' $ POSIXTime $ bpp.start + bpp.userLength

  ---- Admin deposits to pool ----
  depositBatchSize <-
    liftM (error "main: Cannot create Natural") $ Natural.fromString "1"
  runContract_ do
    void $
      depositBondedPoolContract
        bondedParams
        depositBatchSize
        []

  waitForWalletChange "USER 1"
  log "Waiting for withdrawing period..."
  countdownTo' $ POSIXTime $ bpp.start + bpp.userLength + bpp.bondingLength

  ---- User 1 withdraws ----
  _ <- runContract_ $
    userWithdrawBondedPoolContract bondedParams

  waitForWalletChange "ADMIN"
  log "Waiting for closing period..."
  countdownTo' $ POSIXTime bpp.end

  -- Admin closes pool
  closeBatchSize <-
    liftM (error "Cannot create Natural") $ Natural.fromString "10"
  runContract_ do
    void $ closeBondedPoolContract bondedParams closeBatchSize []
    logInfo' "main: Pool closed"

-- Unbonded: admin create pool, user stake, admin deposit (rewards),
-- user withdraw, admin close using PureScript (non SDK)
-- main :: Effect Unit
-- main = launchAff_ do
--   adminCfg <- mkConfig
--   ---- Admin creates pool ----
--   unbondedParams@(UnbondedPoolParams upp) <-
--     runContract adminCfg do
--       logInfo' "STARTING AS ADMIN"
--       initParams <- liftContractM "main: Cannot initiate bonded parameters"
--         testInitUnbondedParams
--       -- We get the current time and set up the pool to start 80 seconds from now
--       POSIXTime currTime <- currentRoundedTime
--       let iup = unwrap initParams
--           poolDelay = 80_000
--           iupWithTime :: InitialUnbondedParams
--           iupWithTime = InitialUnbondedParams $ iup {
--             start = currTime + BigInt.fromInt poolDelay
--           }
--       logInfo_ "Pool creation time" currTime
--       unbondedParams <- createUnbondedPoolContract iupWithTime
--       logInfo_ "Pool parameters" unbondedParams
--       logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
--       -- We give 30 seconds of margin for the users and admin to sign the transactions
--       liftAff $ delay $ wrap $ Int.toNumber $ poolDelay + 30_000
--       pure unbondedParams
--   ---- User 1 deposits ----
--   userCfg <- mkConfig
--   userStake <- liftM (error "main: Cannot create userStake from String") $
--     Natural.fromString "4000"
--   runContract_ userCfg do
--     userStakeUnbondedPoolContract unbondedParams userStake
--     logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--     -- Wait until bonding period
--     liftAff $ delay $ wrap $ BigInt.toNumber upp.userLength
--   -- ---- User 2 deposits ----
--   -- userCfg <- mkConfig
--   -- userStake <- liftM (error "main: Cannot create userStake from String") $
--   --   Natural.fromString "8000000"
--   -- runContract_ userCfg do
--   --   userStakeUnbondedPoolContract unbondedParams userStake
--   --   logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--   --   -- Wait until bonding period
--   --   liftAff $ delay $ wrap $ BigInt.toNumber upp.userLength
--   ---- Admin deposits to pool ----
--   runContract_ adminCfg do
--     depositBatchSize <-
--       liftM (error "Cannot create Natural") $ Natural.fromString "1"
--     void $ depositUnbondedPoolContract unbondedParams depositBatchSize []
--     -- failedDeposits <- depositUnbondedPoolContract unbondedParams depositBatchSize []
--     -- void $ depositUnbondedPoolContract unbondedParams depositBatchSize failedDeposits
--     -- closeBatchSize <-
--     --   liftM (error "Cannot create Natural") $ Natural.fromString "10"
--     -- void $ closeUnbondedPoolContract unbondedParams closeBatchSize []

--     logInfo' "SWITCH WALLETS NOW - CHANGE TO USER 1"
--     -- Wait until withdrawing period
--     liftAff $ delay $ wrap $ BigInt.toNumber upp.adminLength
--   ---- User 1 withdraws ----
--   runContract_ userCfg do
--     userWithdrawUnbondedPoolContract unbondedParams
--     logInfo' "SWITCH WALLETS NOW - CHANGE TO BACK TO ADMIN"
--     -- Wait until closing period
--     logInfo' "Waiting until admin period to close pool..."
--     liftAff $ delay $ wrap $ BigInt.toNumber upp.userLength
--   -- Admin closes pool
--   runContract_ adminCfg do
--     closeBatchSize <-
--       liftM (error "Cannot create Natural") $ Natural.fromString "10"
--     void $ closeUnbondedPoolContract unbondedParams closeBatchSize []
--     logInfo' "END"
--   -- ---- User 2 withdraws ----
--   -- runContract_ userCfg do
--   --   userWithdrawUnbondedPoolContract unbondedParams

-- Bonded: admin create pool, user stake, admin deposit (rewards), admin close
-- using PureScript (SDK)
-- Run *one* at a time:
-- main :: Effect Unit
-- main =
-- bondedCallContractCreatePoolExample1
-- After running the above contract, update `testBondedPoolArgs` accordingly.
-- bondedCallContractUserStakeExample1
-- bondedCallContractAdminDepositExample1
-- bondedCallContractAdminCloseExample1

---- Utils ----

-- | Run contract in an environment defined with a certain `ConfigParams`.
-- | The environment is created and destroyed for this single contract
-- | execution
runContract_ :: forall (a :: Type). Contract () a -> Aff a
runContract_ contract = do
  runContract
    { ogmiosConfig: defaultOgmiosWsConfig
    , datumCacheConfig: defaultDatumCacheWsConfig
    , ctlServerConfig: Just defaultServerConfig
    , networkId: TestnetId
    , logLevel: Info
    , walletSpec: Just ConnectToNami
    , customLogger: Nothing
    , suppressLogs: false
    , extraConfig: {}
    }
    contract

-- | Wait until the given time. It queries the node time every 5 seconds to
-- | determine the passing of time, so more time may actually pass
countdownTo' :: forall (r :: Row Type). POSIXTime -> Aff Unit
countdownTo' = runContract_ <<< countdownTo

-- | Wait for a given number of seconds. This uses LOCAL time, and it's only
-- | used to give the tester some time to switch their wallet
waitN :: Int -> Aff Unit
waitN n =
  if n < 5 then delay' n *> log "GO"
  else delay' 5 *> log (show $ n - 5) *> waitN (n - 5)
  where
  delay' :: Int -> Aff Unit
  delay' = delay <<< Milliseconds <<< Int.toNumber <<< (_ * 1000)
