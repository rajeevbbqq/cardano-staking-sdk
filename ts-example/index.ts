// This module demonstrates how to use the `singularitynet` TS/JS SDK to
// operate a bonded pool across the entire application lifecycle
import {
  SdkConfig,
  BondedPool,
  InitialUnbondedArgs,
  BondedPoolArgs,
  UnbondedPool,
  UnbondedPoolArgs,
} from "singularitynet";

const singularitynet = require("singularitynet");
// We need to use `big-integer` directly instead of the `BigInt` constructor
// as this package defines some methods that are required by the PS code
import BigInteger = require("big-integer");

// Runs all of the operations defined for a `BondedPool` and simulates the
// entire pool lifecycle
//
// Make sure to switch wallets as directed
const main = async () => {
  // some helpers for logging directions to switch wallets
  const admin = "ADMIN";
  const user = "USER";

  // Admin creates pool
  console.log(`STARTING AS ${admin}`);
  const nodeTime = await singularitynet.getNodeTime(localHostSdkConfig);
  console.log(nodeTime);
  const date = new Date(nodeTime);
  const delay = BigInteger(80000);
  console.log(
    `Bonded pool creation: ${date.getHours()}:${date.getMinutes()}:${date.getSeconds()}`
  );
  // Length of all periods (staking/withdraing, bonding and admin)
  const periodLength = BigInteger(180000);

  // The initial arguments of the pool. The rest of the parameters are obtained
  // during pool creation.
  const decimals = 8;
  const initialUnbondedArgs: InitialUnbondedArgs = {
    start: nodeTime.add(delay),
    userLength: BigInteger(15 * 60 * 1000),
    bondingLength: BigInteger(15 * 60 * 1000),
    interest: { numerator: BigInteger(10), denominator: BigInteger(100) },
    minStake: BigInteger(1).times(10 ** decimals),
    maxStake: BigInteger(50000).times(10 ** decimals),
    adminLength: BigInteger(5 * 60 * 1000),
    interestLength: BigInteger(15 * 60 * 1000),
    increments: BigInteger(1),
    unbondedAssetClass: {
      currencySymbol:
        "6f1a1f0c7ccf632cc9ff4b79687ed13ffe5b624cce288b364ebdce50",
      tokenName: "AGIX",
    },
  };

  let unbondedPool: UnbondedPool = await singularitynet.createUnbondedPool(
    localHostSdkConfig,
    initialUnbondedArgs
  );
  const unbondedPoolArgs: UnbondedPoolArgs = unbondedPool.args;
  console.log(JSON.stringify(unbondedPool))

  // We try to recreate the pool just from its address and initial bonded args.
  // This is just for testing the pool query functionality.
  //
  // Note that more than one pool may be found at a given address, since it's
  // possible to create many pools with the same `InitialBondedArgs`. But this
  // is unlikely if the `start` parameter is different for every pool.
  const unbondedPoolsArray: Array<UnbondedPool> = await singularitynet.getUnbondedPools(
    localHostSdkConfig,
    unbondedPool.address,
    initialUnbondedArgs
  );

  // We get the first pool of the list (which should be the only one)
  // and we use it instead of the first object. This should not make a
  // difference, the objects are identical.
  const unbondedPoolCopy = unbondedPoolsArray[0];
  unbondedPool = unbondedPoolCopy;

  // User stakes, waiting for pool start
  await logSwitchAndCountdown(user, "User 1 deposit 1000 AGIX in window 1", unbondedPoolArgs.start);
  const userStakeAmt = BigInteger(1000).times(10 ** decimals);
  const r0 = await unbondedPool.userStake(userStakeAmt);
  console.log(JSON.stringify(r0))

  // await logSwitchAndCountdown(user, "User 2 deposit 2000 AGIX in window 1", unbondedPoolArgs.start);
  // const user2StakeAmt = BigInteger(2000).times(10 ** decimals);
  // const r02 = await unbondedPool.userStake(user2StakeAmt);
  // console.log(JSON.stringify(r02))

  // // Admin deposits to pool, waiting for userLength to end
  // await logSwitchAndCountdown(
  //   admin,
  //   "Admin deposit for window 1",
  //   unbondedPoolArgs.start.add(unbondedPoolArgs.userLength)
  // );
  const r1 = await unbondedPool.deposit(BigInteger(1), []);
  console.log(JSON.stringify(r1));

  // // User withdraws during bonding period, waiting for adminLength to finish
  // await logSwitchAndCountdown(
  //   user,
  //   "User 1 withdraws",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength)
  // );
  // const r2 = await unbondedPool.userWithdraw();
  // console.log(JSON.stringify(r2));

  // // User stakes during user period, waiting for bondingLength to finish
  // await logSwitchAndCountdown(
  //   user,
  //   "User 1 deposit 1500 AGIX in window 2",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength).add(
  //         unbondedPoolArgs.bondingLength)
  // );
  // const window2Deposit = BigInteger(1500).times(10 ** decimals)
  // const r3 = await unbondedPool.userStake(window2Deposit);
  // console.log(JSON.stringify(r3));

  // await logSwitchAndCountdown(
  //   user,
  //   "User 2 deposits 2500 AGIX in window 2",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength).add(
  //         unbondedPoolArgs.bondingLength)
  // );
  // const window2Deposit2 = BigInteger(2500).times(10 ** decimals)
  // const r32 = await unbondedPool.userStake(window2Deposit2);
  // console.log(JSON.stringify(r32));

  // // Admin deposits to window 2, waiting for userLength to end
  // await logSwitchAndCountdown(
  //   admin,
  //   "Admin deposit for window 2",
  //   unbondedPoolArgs.start.add(unbondedPoolArgs.userLength).add(unbondedPoolArgs.adminLength).add(unbondedPoolArgs.bondingLength).add(unbondedPoolArgs.userLength)
  // );
  // const window2depositBatchSize = BigInteger(2);
  // const w2r1 = await unbondedPool.deposit(window2depositBatchSize, []);
  // console.log(JSON.stringify(w2r1));

  // // User withdraws on window 2 during bonding period, waiting for adminLength to finish
  // await logSwitchAndCountdown(
  //   user,
  //   "User 1 withdraws on window 2",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength).add(
  //         unbondedPoolArgs.bondingLength).add(
  //           unbondedPoolArgs.userLength).add(
  //             unbondedPoolArgs.adminLength)
  // );
  // const w2r2 = await unbondedPool.userWithdraw();
  // console.log(JSON.stringify(w2r2));

  // // User withdraws on window 2 during bonding period, waiting for adminLength to finish
  // await logSwitchAndCountdown(
  //   user,
  //   "User 2 withdraws on window 2",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength).add(
  //         unbondedPoolArgs.bondingLength).add(
  //           unbondedPoolArgs.userLength).add(
  //             unbondedPoolArgs.adminLength)
  // );
  // const w2r3 = await unbondedPool.userWithdraw();
  // console.log(JSON.stringify(w2r3));

  // // Admin closes pool, waiting for userLength to finish
  // await logSwitchAndCountdown(
  //   admin,
  //   "admin period",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength).add(
  //         unbondedPoolArgs.bondingLength).add(
  //           unbondedPoolArgs.userLength).add(
  //             unbondedPoolArgs.userLength).add(
  //               unbondedPoolArgs.adminLength).add(
  //                 unbondedPoolArgs.bondingLength).add(
  //                   unbondedPoolArgs.userLength));

  // const closeBatchSize = BigInteger(10);
  // const r4 = await unbondedPool.close(closeBatchSize, []);
  // console.log(JSON.stringify(r4));

  // // The user withdraws their rewards after pool closure.
  // await logSwitchAndCountdown(
  //   admin,
  //   "admin period",
  //   unbondedPoolArgs.start.add(
  //     unbondedPoolArgs.userLength).add(
  //       unbondedPoolArgs.adminLength).add(
  //         unbondedPoolArgs.bondingLength).add(
  //           unbondedPoolArgs.userLength).add(
  //             unbondedPoolArgs.adminLength));

  // const r5 = await unbondedPool.userWithdraw();
  // console.log(JSON.stringify(r5));

};

const localHostSdkConfig: SdkConfig = {
  ctlServerConfig: {
    host: "35.175.138.251",
    port: 8081,
    secure: false,
    path: "",
  },
  ogmiosConfig: {
    host: "35.175.138.251",
    port: 1337,
    secure: false,
    path: "",
  },
  datumCacheConfig: {
    host: "35.175.138.251",
    port: 9999,
    secure: false,
    path: "",
  },
  networkId: 0,
  logLevel: "Info",
  walletSpec: "Eternl",
};

// Helpers

const logSwitchAndCountdown = async (
  who: "USER" | "ADMIN", // the wallet to switch to
  what: string, // what we're waiting for
  time: BigInteger // how long to wait
) => {
  console.log(`SWITCH WALLETS NOW - CHANGE TO ${who}`);
  const _input = prompt("Press OK after switching.");
  console.log(`Waiting for ${what}...`);
  await countdownTo(Number(time));
};

const countdownTo = async (tf: number) => {
  let now = await singularitynet.getNodeTime(localHostSdkConfig);
  while (now <= tf) {
    console.log(`${now} <= ${tf}`);
    console.log(`Countdown: ${showSecondsDiff(tf, now)}`);
    await sleep(10000);
    now = await singularitynet.getNodeTime(localHostSdkConfig);
  }
  console.log(`${now} > ${tf}`);
  console.log(`0`);
};

const sleep = async (ms: number) => new Promise((r) => setTimeout(r, ms));

const showSecondsDiff = (x: number, y: number) => (x - y) / 1000;

// Run the contracts, see `main` above
main()
  .then((_) => { })
  .catch((e) => console.log(e));