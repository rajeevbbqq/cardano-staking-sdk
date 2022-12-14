// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Aeson = require("../Aeson/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_Char = require("../Data.Char/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Test_QuickCheck_Arbitrary = require("../Test.QuickCheck.Arbitrary/index.js");
var Test_QuickCheck_Gen = require("../Test.QuickCheck.Gen/index.js");
var ByteArray = function (x) {
    return x;
};
var showByteArray = {
    show: function (arr) {
        return "(hexToByteArrayUnsafe " + (Data_Show.show(Data_Show.showString)($foreign.byteArrayToHex(arr)) + ")");
    }
};
var semigroupByteArray = {
    append: $foreign.concat_
};
var newtypeByteArray_ = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidByteArray = {
    mempty: $foreign.byteArrayFromIntArrayUnsafe([  ]),
    Semigroup0: function () {
        return semigroupByteArray;
    }
};
var ordByteArray = {
    compare: (function () {
        var toDelta = function (x) {
            return function (y) {
                var v = Data_Ord.compare(Data_Ord.ordInt)(x)(y);
                if (v instanceof Data_Ordering.EQ) {
                    return 0;
                };
                if (v instanceof Data_Ordering.LT) {
                    return 1;
                };
                if (v instanceof Data_Ordering.GT) {
                    return -1 | 0;
                };
                throw new Error("Failed pattern match at Types.ByteArray (line 54, column 7 - line 57, column 17): " + [ v.constructor.name ]);
            };
        };
        return function (xs) {
            return function (ys) {
                return Data_Ord.compare(Data_Ord.ordInt)(0)($foreign.ord_(toDelta)(xs)(ys));
            };
        };
    })(),
    Eq0: function () {
        return eqByteArray;
    }
};
var eqByteArray = {
    eq: function (a) {
        return function (b) {
            return Data_Eq.eq(Data_Ordering.eqOrdering)(Data_Ord.compare(ordByteArray)(a)(b))(Data_Ordering.EQ.value);
        };
    }
};
var encodeAesonByteArray = {
    "encodeAeson'": function (ba) {
        return Aeson["encodeAeson'"](Aeson.encodeAesonString)($foreign.byteArrayToHex(ba));
    }
};
var arbitraryByteArray = {
    arbitrary: Data_Functor.map(Test_QuickCheck_Gen.functorGen)($foreign.byteArrayFromIntArrayUnsafe)(Test_QuickCheck_Arbitrary.arbitrary(Test_QuickCheck_Arbitrary.arbArray(Test_QuickCheck_Arbitrary.arbInt)))
};
var hexToByteArray = $foreign.hexToByteArray_(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
var decodeAesonByteArray = {
    decodeAeson: function (j) {
        var unexpectedValueError = Data_Argonaut_Decode_Error.UnexpectedValue.create(Aeson.toStringifiedNumbersJson(j));
        var typeMismatchError = new Data_Argonaut_Decode_Error.TypeMismatch("expected a hex-encoded string");
        return Aeson.caseAesonString(new Data_Either.Left(typeMismatchError))((function () {
            var $13 = Data_Either.note(unexpectedValueError);
            return function ($14) {
                return $13(hexToByteArray($14));
            };
        })())(j);
    }
};
var byteArrayFromIntArray = $foreign.byteArrayFromIntArray_(Data_Maybe.Nothing.value)(Data_Maybe.Just.create);
var byteArrayFromAscii = function (str) {
    return Data_Functor.map(Data_Maybe.functorMaybe)($foreign.byteArrayFromIntArrayUnsafe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(Data_String_CodeUnits.toCharArray(str))(function (cp) {
        var charCode = Data_Char.toCharCode(cp);
        var $12 = charCode <= 255 && charCode >= 0;
        if ($12) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(charCode);
        };
        return Data_Maybe.Nothing.value;
    }));
};
module.exports = {
    ByteArray: ByteArray,
    byteArrayFromIntArray: byteArrayFromIntArray,
    byteArrayFromAscii: byteArrayFromAscii,
    hexToByteArray: hexToByteArray,
    newtypeByteArray_: newtypeByteArray_,
    showByteArray: showByteArray,
    eqByteArray: eqByteArray,
    ordByteArray: ordByteArray,
    semigroupByteArray: semigroupByteArray,
    monoidByteArray: monoidByteArray,
    decodeAesonByteArray: decodeAesonByteArray,
    encodeAesonByteArray: encodeAesonByteArray,
    arbitraryByteArray: arbitraryByteArray,
    byteArrayFromIntArrayUnsafe: $foreign.byteArrayFromIntArrayUnsafe,
    byteArrayFromInt16ArrayUnsafe: $foreign.byteArrayFromInt16ArrayUnsafe,
    byteArrayToHex: $foreign.byteArrayToHex,
    byteArrayToIntArray: $foreign.byteArrayToIntArray,
    byteLength: $foreign.byteLength,
    hexToByteArrayUnsafe: $foreign.hexToByteArrayUnsafe,
    byteArrayToUTF16le: $foreign.byteArrayToUTF16le,
    subarray: $foreign.subarray
};
