// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Aeson = require("../Aeson/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_UInt = require("../Data.UInt/index.js");
var Deserialization_Error = require("../Deserialization.Error/index.js");
var $$Error = require("../Error/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var showBigNum = {
    show: function (bn) {
        return "fromString \"" + ($foreign.toString(bn) + "\"");
    }
};
var eqBigNum = {
    eq: function (lhs) {
        return function (rhs) {
            return $foreign.bnCompare(lhs)(rhs) === 0;
        };
    }
};
var ordBigNum = {
    compare: function (lhs) {
        return function (rhs) {
            var v = $foreign.bnCompare(lhs)(rhs);
            if (v === 1) {
                return Data_Ordering.GT.value;
            };
            if (v === 0) {
                return Data_Ordering.EQ.value;
            };
            return Data_Ordering.LT.value;
        };
    },
    Eq0: function () {
        return eqBigNum;
    }
};
var toUInt = function ($10) {
    return Data_UInt.fromString($foreign.toString($10));
};
var toInt = function ($11) {
    return Data_Int.fromString($foreign.toString($11));
};
var toInt$prime = function (nm) {
    return function (bn) {
        return $$Error.noteE(Deserialization_Error.fromCslRepError(nm + (": CSL.BigNum (" + (Data_Show.show(showBigNum)(bn) + ") -> Int "))))(toInt(bn));
    };
};
var toBigInt = function ($12) {
    return Data_BigInt.fromString($foreign.toString($12));
};
var toBigInt$prime = function (nm) {
    return function (bn) {
        return $$Error.noteE(Deserialization_Error.fromCslRepError(nm + (": CSL.BigNum (" + (Data_Show.show(showBigNum)(bn) + ") -> BigInt "))))(toBigInt(bn));
    };
};
var toBigIntUnsafe = (function () {
    var $13 = Data_Maybe.fromJust();
    return function ($14) {
        return $13(toBigInt($14));
    };
})();
var encodeAesonBigNum = {
    "encodeAeson'": (function () {
        var $15 = Aeson["encodeAeson'"](Aeson.encodeAesonBigInt);
        return function ($16) {
            return $15(toBigIntUnsafe($16));
        };
    })()
};
var mul = $foreign.bnMul(FfiHelpers.maybeFfiHelper);
var fromString = $foreign["_fromString"](FfiHelpers.maybeFfiHelper);
var fromStringUnsafe = (function () {
    var $17 = Data_Maybe.fromJust();
    return function ($18) {
        return $17(fromString($18));
    };
})();
var fromUInt = function ($19) {
    return fromStringUnsafe(Data_UInt.toString($19));
};
var maxValue = fromStringUnsafe("18446744073709551615");
var fromInt = function ($20) {
    return fromStringUnsafe(Data_UInt.toString(Data_UInt.fromInt($20)));
};
var fromBigInt = function ($21) {
    return fromString(Data_BigInt.toString($21));
};
var decodeAesonBigNum = {
    decodeAeson: Control_Bind.composeKleisliFlipped(Data_Either.bindEither)((function () {
        var $22 = Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("Couldn't convert `BigInt` to `BigNum`"));
        return function ($23) {
            return $22(fromBigInt($23));
        };
    })())(Aeson.decodeAeson(Aeson.decodeAesonBigInt))
};
var add = $foreign.bnAdd(FfiHelpers.maybeFfiHelper);
module.exports = {
    add: add,
    fromBigInt: fromBigInt,
    fromInt: fromInt,
    fromString: fromString,
    fromStringUnsafe: fromStringUnsafe,
    maxValue: maxValue,
    mul: mul,
    toBigInt: toBigInt,
    "toBigInt'": toBigInt$prime,
    toBigIntUnsafe: toBigIntUnsafe,
    toInt: toInt,
    "toInt'": toInt$prime,
    fromUInt: fromUInt,
    toUInt: toUInt,
    eqBigNum: eqBigNum,
    ordBigNum: ordBigNum,
    showBigNum: showBigNum,
    decodeAesonBigNum: decodeAesonBigNum,
    encodeAesonBigNum: encodeAesonBigNum,
    one: $foreign.one,
    toString: $foreign.toString,
    zero: $foreign.zero
};
