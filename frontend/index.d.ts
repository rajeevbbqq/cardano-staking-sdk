import { BigInteger } from "big-integer";

export declare class Pool<T> {
  readonly config: SdkConfig;
  readonly args: T;
  readonly address: string;

  constructor(config: SdkConfig, args: T, address: string);

  deposit(amount: BigInteger, idxArray: int[]): Promise<int[]>;
  close(amount: BigInteger, idxArray: int[]): Promise<int[]>;
  userStake(amount: BigInteger): Promise<any>;
  userWithdraw(): Promise<any>;
}

export type LogLevel = "Trace" | "Debug" | "Info" | "Warn" | "Error";

export type WalletSpec = "Nami" | "Gero" | "Flint" | "Lode" | "Eternl";

export type NetworkId = 0 | 1;

export type SdkServerConfig = {
  host: string; // e.g. "localhost"
  port: number; // uint
  path: string; // leave empty to unset
  secure: boolean;
};

export type SdkAssetClass = {
  currencySymbol: string;
  tokenName: string;
};

export type SdkInterest = {
  numerator: BigInteger;
  denominator: BigInteger;
};

export type SdkConfig = {
  ctlServerConfig: SdkServerConfig;
  ogmiosConfig: SdkServerConfig;
  datumCacheConfig: SdkServerConfig;
  networkId: NetworkId; // int
  logLevel: LogLevel;
  walletSpec: WalletSpec;
};

// Bonded pool

export declare class BondedPool extends Pool<BondedPoolArgs> {}

export declare function createBondedPool(
  config: SdkConfig,
  initialArgs: InitialBondedArgs
): Promise<BondedPool>;

export declare function getBondedPools(
  config: SdkConfig,
  address: string,
  initialArgs: InitialBondedArgs
): Promise<Array<BondedPool>>;

export type BondedPoolArgs = {
  iterations: BigInteger; // Natural
  start: BigInteger; // like POSIXTime so positive
  end: BigInteger; // like POSIXTime so positive
  userLength: BigInteger; // like POSIXTime so positive
  bondingLength: BigInteger; // like POSIXTime so positive
  interest: SdkInterest;
  minStake: BigInteger; // Natural
  maxStake: BigInteger; // Natural
  bondedAssetClass: SdkAssetClass;
  admin: string; // PaymentPubKeyHash
  nftCs: string; // CBORHexCurrencySymbol
  assocListCs: string; // CBORHexCurrencySymbol
};

export type InitialBondedArgs = {
  iterations: BigInteger; // Natural
  start: BigInteger; // like POSIXTime so positive
  end: BigInteger; // like POSIXTime so positive
  userLength: BigInteger; // like POSIXTime so positive
  bondingLength: BigInteger; // like POSIXTime so positive
  interest: SdkInterest;
  minStake: BigInteger; // Natural
  maxStake: BigInteger; // Natural
  bondedAssetClass: SdkAssetClass;
};

// Unbonded pool

export declare class UnbondedPool extends Pool<UnbondedPoolArgs> {}

export declare function createUnbondedPool(
  config: SdkConfig,
  initialArgs: InitialUnbondedArgs
): Promise<UnbondedPool>;

export declare function getUnbondedPools(
  config: SdkConfig,
  address: string,
  initialArgs: InitialUnbondedArgs
): Promise<Array<UnbondedPool>>;

export type UnbondedPoolArgs = {
  start: BigInteger; // like POSIXTime so positive
  userLength: BigInteger; // like POSIXTime so positive
  bondingLength: BigInteger; // like POSIXTime so positive
  adminLength: BigInteger; // like POSIXTime so positive
  interestLength: BigInteger; // like POSIXTime so positive
  increments: BigInteger; // Natural
  interest: SdkInterest;
  minStake: BigInteger; // Natural
  maxStake: BigInteger; // Natural
  unbondedAssetClass: SdkAssetClass;
  admin: string; // PaymentPubKeyHash
  nftCs: string; // CBORHexCurrencySymbol
  assocListCs: string; // CBORHexCurrencySymbol
};

export type InitialUnbondedArgs = {
  start: BigInteger; // like POSIXTime so positive
  userLength: BigInteger; // like POSIXTime so positive
  adminLength: BigInteger; // like POSIXTime so positive
  interestLength: BigInteger; // like POSIXTime so positive
  bondingLength: BigInteger; // like POSIXTime so positive
  increments: BigInteger; // Natural
  interest: SdkInterest;
  minStake: BigInteger; // Natural
  maxStake: BigInteger; // Natural
  unbondedAssetClass: SdkAssetClass;
};

export declare function getNodeTime(config: SdkConfig): Promise<BigInteger>;
