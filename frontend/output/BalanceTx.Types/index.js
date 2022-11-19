// Generated by purs version 0.14.5
"use strict";
var Cardano_Types_Transaction = require("../Cardano.Types.Transaction/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Types_ScriptLookups = require("../Types.ScriptLookups/index.js");
var PrebalancedTransaction = function (x) {
    return x;
};
var FinalizedTransaction = function (x) {
    return x;
};
var newtypePrebalancedTransac = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeFinalizedTransacti = {
    Coercible0: function () {
        return undefined;
    }
};
var genericPrebalancedTransac = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showPrebalancedTransactio = {
    show: Data_Show_Generic.genericShow(genericPrebalancedTransac)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Types_ScriptLookups.showUnattachedUnbalancedT))({
        reflectSymbol: function () {
            return "PrebalancedTransaction";
        }
    }))
};
var genericFinalizedTransacti = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showFinalizedTransaction = {
    show: Data_Show_Generic.genericShow(genericFinalizedTransacti)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Cardano_Types_Transaction.showTransaction))({
        reflectSymbol: function () {
            return "FinalizedTransaction";
        }
    }))
};
var eqFinalizedTransaction = Cardano_Types_Transaction.eqTransaction;
module.exports = {
    FinalizedTransaction: FinalizedTransaction,
    PrebalancedTransaction: PrebalancedTransaction,
    genericFinalizedTransacti: genericFinalizedTransacti,
    newtypeFinalizedTransacti: newtypeFinalizedTransacti,
    eqFinalizedTransaction: eqFinalizedTransaction,
    showFinalizedTransaction: showFinalizedTransaction,
    genericPrebalancedTransac: genericPrebalancedTransac,
    newtypePrebalancedTransac: newtypePrebalancedTransac,
    showPrebalancedTransactio: showPrebalancedTransactio
};
