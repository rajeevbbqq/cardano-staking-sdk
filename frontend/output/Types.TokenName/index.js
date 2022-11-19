// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Aeson = require("../Aeson/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_Bitraversable = require("../Data.Bitraversable/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_CodePoints = require("../Data.String.CodePoints/index.js");
var Data_TextEncoding = require("../Data.TextEncoding/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var FromData = require("../FromData/index.js");
var ToData = require("../ToData/index.js");
var Types_ByteArray = require("../Types.ByteArray/index.js");
var Types_RawBytes = require("../Types.RawBytes/index.js");
var TokenName = function (x) {
    return x;
};
var toMetadataTokenName = Types_RawBytes.toMetadataRawBytes;
var toDataTokenName = ToData.toDataRawBytes;
var showTokenName = {
    show: function (v) {
        return "(TokenName " + (Data_Show.show(Types_RawBytes.showRawBytes)(v) + ")");
    }
};
var ordTokenName = Types_RawBytes.ordRawBytes;
var fromMetadataTokenName = Types_RawBytes.fromMetadataRawBytes;
var fromDataTokenName = FromData.fromDataRawBytes;
var eqTokenName = Types_RawBytes.eqRawBytes;
var decodeAesonTokenName = {
    decodeAeson: (function () {
        var tkFromStr = (function () {
            var $26 = Data_Newtype.wrap();
            var $27 = Data_Newtype.wrap();
            return function ($28) {
                return TokenName($26($27(Data_TextEncoding.encodeUtf8($28))));
            };
        })();
        return Aeson.caseAesonObject(Data_Either.Left.create(new Data_Argonaut_Decode_Error.TypeMismatch("Expected object")))(function (aes) {
            return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonString)(aes)("unTokenName"))(function (tkstr) {
                var v = Data_String_CodePoints.take(3)(tkstr);
                if (v === "\x000x") {
                    var stripped = Data_String_CodePoints.drop(3)(tkstr);
                    return Control_Bind.bind(Data_Either.bindEither)(Data_Either.note(Data_Argonaut_Decode_Error.TypeMismatch.create("Expected base16 encoded string got " + stripped))(Types_ByteArray.hexToByteArray(stripped)))(function (ba) {
                        return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Newtype.wrap()(ba));
                    });
                };
                if (v === "\x00\x00\x00") {
                    return Data_Either.Right.create(tkFromStr(Data_String_CodePoints.drop(2)(tkstr)));
                };
                return Data_Either.Right.create(tkFromStr(tkstr));
            });
        });
    })()
};
var tokenNameFromAssetName = (function () {
    var $29 = Data_Newtype.wrap();
    return function ($30) {
        return TokenName($29($foreign.assetNameName($30)));
    };
})();
var mkTokenName = function (byteArr) {
    if (Types_ByteArray.byteLength(byteArr) <= 32) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(TokenName(Data_Newtype.wrap()(byteArr)));
    };
    if (Data_Boolean.otherwise) {
        return Data_Maybe.Nothing.value;
    };
    throw new Error("Failed pattern match at Types.TokenName (line 100, column 1 - line 100, column 44): " + [ byteArr.constructor.name ]);
};
var mkTokenNames = function (dictTraversable) {
    var $31 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Map_Internal.fromFoldable(ordTokenName)(dictTraversable.Foldable1()));
    var $32 = Data_Traversable.traverse(dictTraversable)(Data_Maybe.applicativeMaybe)(Data_Bitraversable.ltraverse(Data_Bitraversable.bitraversableTuple)(Data_Maybe.applicativeMaybe)(mkTokenName));
    return function ($33) {
        return $31($32($33));
    };
};
var getTokenName = function (v) {
    return Data_Newtype.unwrap()(v);
};
var fromTokenName = function (arrayHandler) {
    return function (stringHandler) {
        return function (v) {
            return Data_Either.either(Data_Function["const"](arrayHandler(v)))(stringHandler)($foreign["_decodeUtf8"](Data_Newtype.unwrap()(v))(Data_Either.Left.create)(Data_Either.Right.create));
        };
    };
};
var encodeAesonTokenName = {
    "encodeAeson'": (function () {
        var $34 = Aeson["encodeAeson'"](Aeson.encodeAesonRecord(Aeson.gEncodeAesonCons(Aeson.encodeAesonString)(Aeson.gEncodeAesonNil)({
            reflectSymbol: function () {
                return "unTokenName";
            }
        })())());
        var $35 = fromTokenName(function (ba) {
            return "\x00" + ("0x" + Types_ByteArray.byteArrayToHex(ba));
        })(function (s) {
            var v = Data_String_CodePoints.take(1)(s);
            if (v === "\x00") {
                return "\x00\x00" + s;
            };
            return s;
        });
        return function ($36) {
            return $34((function (v) {
                return {
                    unTokenName: v
                };
            })($35($36)));
        };
    })()
};
var adaToken = Data_Monoid.mempty(Types_RawBytes.monoidRawBytes);
module.exports = {
    adaToken: adaToken,
    getTokenName: getTokenName,
    mkTokenName: mkTokenName,
    mkTokenNames: mkTokenNames,
    tokenNameFromAssetName: tokenNameFromAssetName,
    eqTokenName: eqTokenName,
    fromDataTokenName: fromDataTokenName,
    fromMetadataTokenName: fromMetadataTokenName,
    toMetadataTokenName: toMetadataTokenName,
    ordTokenName: ordTokenName,
    toDataTokenName: toDataTokenName,
    decodeAesonTokenName: decodeAesonTokenName,
    encodeAesonTokenName: encodeAesonTokenName,
    showTokenName: showTokenName,
    assetNameName: $foreign.assetNameName
};
