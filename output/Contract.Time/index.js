// Generated by purs version 0.14.5
"use strict";
var Contract_Chain = require("../Contract.Chain/index.js");
var Contract_Monad = require("../Contract.Monad/index.js");
var QueryM_CurrentEpoch = require("../QueryM.CurrentEpoch/index.js");
var QueryM_EraSummaries = require("../QueryM.EraSummaries/index.js");
var QueryM_Ogmios = require("../QueryM.Ogmios/index.js");
var QueryM_SystemStart = require("../QueryM.SystemStart/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var Types_Interval = require("../Types.Interval/index.js");
var getSystemStart = Contract_Monad.wrapContract(QueryM_SystemStart.getSystemStart);
var getEraSummaries = Contract_Monad.wrapContract(QueryM_EraSummaries.getEraSummaries);
var getCurrentEpoch = Contract_Monad.wrapContract(QueryM_CurrentEpoch.getCurrentEpoch);
module.exports = {
    getCurrentEpoch: getCurrentEpoch,
    getEraSummaries: getEraSummaries,
    getSystemStart: getSystemStart,
    BlockHeaderHash: Contract_Chain.BlockHeaderHash,
    ChainTip: Contract_Chain.ChainTip,
    Tip: Contract_Chain.Tip,
    TipAtGenesis: Contract_Chain.TipAtGenesis,
    getTip: Contract_Chain.getTip,
    CurrentEpoch: QueryM_Ogmios.CurrentEpoch,
    Epoch: QueryM_Ogmios.Epoch,
    EpochLength: QueryM_Ogmios.EpochLength,
    EraSummaries: QueryM_Ogmios.EraSummaries,
    EraSummary: QueryM_Ogmios.EraSummary,
    EraSummaryParameters: QueryM_Ogmios.EraSummaryParameters,
    RelativeTime: QueryM_Ogmios.RelativeTime,
    SafeZone: QueryM_Ogmios.SafeZone,
    SlotLength: QueryM_Ogmios.SlotLength,
    SystemStart: QueryM_Ogmios.SystemStart,
    BlockId: Serialization_Address.BlockId,
    Slot: Serialization_Address.Slot,
    AbsTime: Types_Interval.AbsTime,
    Finite: Types_Interval.Finite,
    NegInf: Types_Interval.NegInf,
    PosInf: Types_Interval.PosInf,
    Interval: Types_Interval.Interval,
    LowerBound: Types_Interval.LowerBound,
    ModTime: Types_Interval.ModTime,
    OnchainPOSIXTimeRange: Types_Interval.OnchainPOSIXTimeRange,
    POSIXTime: Types_Interval.POSIXTime,
    CannotFindTimeInEraSummaries: Types_Interval.CannotFindTimeInEraSummaries,
    "CannotGetBigIntFromNumber'": Types_Interval["CannotGetBigIntFromNumber'"],
    EndSlotLessThanSlotOrModNonZero: Types_Interval.EndSlotLessThanSlotOrModNonZero,
    PosixTimeBeforeSystemStart: Types_Interval.PosixTimeBeforeSystemStart,
    StartTimeGreaterThanTime: Types_Interval.StartTimeGreaterThanTime,
    RelTime: Types_Interval.RelTime,
    CannotFindSlotInEraSummaries: Types_Interval.CannotFindSlotInEraSummaries,
    CannotGetBigIntFromNumber: Types_Interval.CannotGetBigIntFromNumber,
    EndTimeLessThanTime: Types_Interval.EndTimeLessThanTime,
    StartingSlotGreaterThanSlot: Types_Interval.StartingSlotGreaterThanSlot,
    "PosixTimeToSlotError'": Types_Interval["PosixTimeToSlotError'"],
    "SlotToPosixTimeError'": Types_Interval["SlotToPosixTimeError'"],
    UpperBound: Types_Interval.UpperBound,
    after: Types_Interval.after,
    always: Types_Interval.always,
    before: Types_Interval.before,
    beginningOfTime: Types_Interval.beginningOfTime,
    contains: Types_Interval.contains,
    findSlotEraSummary: Types_Interval.findSlotEraSummary,
    findTimeEraSummary: Types_Interval.findTimeEraSummary,
    from: Types_Interval.from,
    hull: Types_Interval.hull,
    intersection: Types_Interval.intersection,
    interval: Types_Interval.interval,
    isEmpty: Types_Interval.isEmpty,
    lowerBound: Types_Interval.lowerBound,
    maxSlot: Types_Interval.maxSlot,
    member: Types_Interval.member,
    mkInterval: Types_Interval.mkInterval,
    never: Types_Interval.never,
    overlaps: Types_Interval.overlaps,
    posixTimeRangeToSlotRange: Types_Interval.posixTimeRangeToSlotRange,
    posixTimeToSlot: Types_Interval.posixTimeToSlot,
    singleton: Types_Interval.singleton,
    slotRangeToPosixTimeRange: Types_Interval.slotRangeToPosixTimeRange,
    slotToPosixTime: Types_Interval.slotToPosixTime,
    strictLowerBound: Types_Interval.strictLowerBound,
    strictUpperBound: Types_Interval.strictUpperBound,
    to: Types_Interval.to,
    toOnchainPosixTimeRange: Types_Interval.toOnchainPosixTimeRange,
    upperBound: Types_Interval.upperBound
};
