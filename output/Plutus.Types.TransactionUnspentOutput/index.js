// Generated by purs version 0.14.5
"use strict";
var Data_Array = require("../Data.Array/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lens_Iso_Newtype = require("../Data.Lens.Iso.Newtype/index.js");
var Data_Lens_Record = require("../Data.Lens.Record/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Plutus_Types_Transaction = require("../Plutus.Types.Transaction/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Types_Transaction = require("../Types.Transaction/index.js");
var TransactionUnspentOutput = function (x) {
    return x;
};
var newtypeTransactionUnspent = {
    Coercible0: function () {
        return undefined;
    }
};
var genericTransactionUnspent = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showTransactionUnspentOut = {
    show: Data_Show_Generic.genericShow(genericTransactionUnspent)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "input";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "output";
        }
    })(Data_Show.showRecordFieldsNil)(Plutus_Types_Transaction.showTransactionOutputWith))(Types_Transaction.showTransactionInput))))({
        reflectSymbol: function () {
            return "TransactionUnspentOutput";
        }
    }))
};
var eqTransactionUnspentOutpu = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "output";
    }
})(Plutus_Types_Transaction.eqTransactionOutputWithRe))()({
    reflectSymbol: function () {
        return "input";
    }
})(Types_Transaction.eqTransactionInput));
var mkTxUnspentOut = function (input) {
    return function (output) {
        return {
            input: input,
            output: output
        };
    };
};
var lookupTxHash = function (txHash) {
    return function (utxos) {
        return Data_Functor.map(Data_Functor.functorArray)(function (v) {
            return {
                input: v.value0,
                output: v.value1
            };
        })(Data_Array.filter((function () {
            var $15 = Data_Eq.eq(Types_Transaction.eqTransactionHash)(txHash);
            var $16 = Data_Newtype.unwrap();
            return function ($17) {
                return $15((function (v) {
                    return v.transactionId;
                })($16(Data_Tuple.fst($17))));
            };
        })())(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(utxos)));
    };
};
var _output = function (dictStrong) {
    var $18 = Data_Lens_Iso_Newtype["_Newtype"]()()(dictStrong.Profunctor0());
    var $19 = Data_Lens_Record.prop({
        reflectSymbol: function () {
            return "output";
        }
    })()()(Type_Proxy["Proxy"].value)(dictStrong);
    return function ($20) {
        return $18($19($20));
    };
};
var _input = function (dictStrong) {
    var $21 = Data_Lens_Iso_Newtype["_Newtype"]()()(dictStrong.Profunctor0());
    var $22 = Data_Lens_Record.prop({
        reflectSymbol: function () {
            return "input";
        }
    })()()(Type_Proxy["Proxy"].value)(dictStrong);
    return function ($23) {
        return $21($22($23));
    };
};
module.exports = {
    TransactionUnspentOutput: TransactionUnspentOutput,
    lookupTxHash: lookupTxHash,
    "_input": _input,
    "_output": _output,
    mkTxUnspentOut: mkTxUnspentOut,
    genericTransactionUnspent: genericTransactionUnspent,
    newtypeTransactionUnspent: newtypeTransactionUnspent,
    eqTransactionUnspentOutpu: eqTransactionUnspentOutpu,
    showTransactionUnspentOut: showTransactionUnspentOut
};
