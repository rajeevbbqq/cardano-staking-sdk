// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Aeson = require("../Aeson/index.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Types_BigNum = require("../Types.BigNum/index.js");
var showInt = {
    show: $foreign["_intToStr"]
};
var eqInt = {
    eq: Data_Function.on(Data_Eq.eq(Data_Eq.eqString))($foreign["_intToStr"])
};
var toBigInt = function ($$int) {
    return Data_Maybe.fromJust()(Data_BigInt.fromString($foreign["_intToStr"]($$int)));
};
var toInt = function ($7) {
    return Data_BigInt.toInt(toBigInt($7));
};
var encodeAesonInt = {
    "encodeAeson'": (function () {
        var $8 = Aeson["encodeAeson'"](Aeson.encodeAesonBigInt);
        return function ($9) {
            return $8(toBigInt($9));
        };
    })()
};
var ordInt = {
    compare: Data_Function.on(Data_Ord.compare(Data_BigInt.ordBigInt))(toBigInt),
    Eq0: function () {
        return eqInt;
    }
};
var fromInt = function (n) {
    if (n < 0) {
        return $foreign.newNegative(Types_BigNum.fromInt(n));
    };
    if (Data_Boolean.otherwise) {
        return $foreign.newPositive(Types_BigNum.fromInt(n));
    };
    throw new Error("Failed pattern match at Types.Int (line 64, column 1 - line 64, column 27): " + [ n.constructor.name ]);
};
var fromBigInt = function (bi) {
    return Control_Alt.alt(Data_Maybe.altMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)($foreign.newPositive)(Types_BigNum.fromBigInt(bi)))(Data_Functor.map(Data_Maybe.functorMaybe)($foreign.newNegative)(Types_BigNum.fromBigInt(Data_Ring.negate(Data_BigInt.ringBigInt)(bi))));
};
var fromString = Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(fromBigInt)(Data_BigInt.fromString);
var decodeAesonInt = {
    decodeAeson: function (aeson) {
        return Control_Bind.bind(Data_Either.bindEither)(Aeson.decodeAeson(Aeson.decodeAesonBigInt)(aeson))((function () {
            var $10 = Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("Int"));
            return function ($11) {
                return $10(fromBigInt($11));
            };
        })());
    }
};
module.exports = {
    fromBigInt: fromBigInt,
    toBigInt: toBigInt,
    fromInt: fromInt,
    toInt: toInt,
    fromString: fromString,
    eqInt: eqInt,
    ordInt: ordInt,
    showInt: showInt,
    encodeAesonInt: encodeAesonInt,
    decodeAesonInt: decodeAesonInt,
    newPositive: $foreign.newPositive,
    newNegative: $foreign.newNegative
};