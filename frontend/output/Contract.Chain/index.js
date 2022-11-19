// Generated by purs version 0.14.5
"use strict";
var Contract_Monad = require("../Contract.Monad/index.js");
var QueryM = require("../QueryM/index.js");
var QueryM_WaitUntilSlot = require("../QueryM.WaitUntilSlot/index.js");
var Types_Chain = require("../Types.Chain/index.js");
var waitUntilSlot = function ($0) {
    return Contract_Monad.wrapContract(QueryM_WaitUntilSlot.waitUntilSlot($0));
};
var waitNSlots = function ($1) {
    return Contract_Monad.wrapContract(QueryM_WaitUntilSlot.waitNSlots($1));
};
var getTip = Contract_Monad.wrapContract(QueryM.getChainTip);
var currentTime = Contract_Monad.wrapContract(QueryM_WaitUntilSlot.currentTime);
var currentSlot = Contract_Monad.wrapContract(QueryM_WaitUntilSlot.currentSlot);
module.exports = {
    getTip: getTip,
    waitUntilSlot: waitUntilSlot,
    waitNSlots: waitNSlots,
    currentTime: currentTime,
    currentSlot: currentSlot,
    BlockHeaderHash: Types_Chain.BlockHeaderHash,
    ChainTip: Types_Chain.ChainTip,
    Tip: Types_Chain.Tip,
    TipAtGenesis: Types_Chain.TipAtGenesis
};
