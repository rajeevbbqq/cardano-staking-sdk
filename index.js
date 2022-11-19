"use strict";

const frontend = import("./output.js");

exports.BondedPool = class BondedPool {
  constructor(config, args, address) {
    this.config = config;
    this.args = args;
    this.address = address;
    this._config = this._getConfig(config);
  }

  async _getConfig(config) {
    const contracts = await frontend;
    const _config = await contracts.buildContractConfig(config)();
    this._config = _config;
  }

  async deposit(amount, idxArray) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callDepositBondedPool(_config)(this.args)(amount)(
      idxArray
    )();
  }

  async close(amount, idxArray) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callCloseBondedPool(_config)(this.args)(amount)(
      idxArray
    )();
  }

  async userStake(amount) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callUserStakeBondedPool(_config)(this.args)(amount)();
  }

  async userWithdraw() {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callUserWithdrawBondedPool(_config)(this.args)();
  }
};

exports.UnbondedPool = class UnbondedPool {
  constructor(config, args, address) {
    this.config = config;
    this.args = args;
    this.address = address;
    this._config = this._getConfig(config);
  }

  async _getConfig(config) {
    const contracts = await frontend;
    const _config = await contracts.buildContractConfig(config)();
    this._config = _config;
  }

  async deposit(amount, idxArray) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callDepositUnbondedPool(_config)(this.args)(amount)(
      idxArray
    )();
  }

  async close(amount, idxArray) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callCloseUnbondedPool(_config)(this.args)(amount)(
      idxArray
    )();
  }

  async userStake(amount) {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callUserStakeUnbondedPool(_config)(this.args)(amount)();
  }

  async userWithdraw() {
    const contracts = await frontend;
    const _config = await this._config;
    return contracts.callUserWithdrawUnbondedPool(_config)(this.args)();
  }
};

exports.createBondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const info = await contracts.callCreateBondedPool(config)(initialArgs)();
  return new exports.BondedPool(sdkConfig, info.args, info.address);
};

exports.getBondedPools = async (sdkConfig, address, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const poolsBondedParams = await contracts.callGetBondedPools(config)(address)(initialArgs)();
  let pools = [];
  for (const bondedParams of poolsBondedParams) {
      pools.push(new exports.BondedPool(sdkConfig, bondedParams, address));
  }
  return pools;
};

exports.createUnbondedPool = async (sdkConfig, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const info = await contracts.callCreateUnbondedPool(config)(initialArgs)();
  return new exports.UnbondedPool(sdkConfig, info.args, info.address);
};

exports.getUnbondedPools = async (sdkConfig, address, initialArgs) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const poolsUnbondedParams = await contracts.callGetUnbondedPools(config)(address)(initialArgs)();
  let pools = [];
  for (const unbondedParams of poolsUnbondedParams) {
      pools.push(new exports.UnbondedPool(sdkConfig, unbondedParams, address));
  }
  return pools;
};

exports.getNodeTime = async (sdkConfig) => {
  const contracts = await frontend;
  const config = await contracts.buildContractConfig(sdkConfig)();
  const time = await contracts.callGetNodeTime(config)();
  return time;
};
