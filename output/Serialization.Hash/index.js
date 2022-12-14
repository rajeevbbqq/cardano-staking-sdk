// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Aeson = require("../Aeson/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var Metadata_ToMetadata = require("../Metadata.ToMetadata/index.js");
var ToData = require("../ToData/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var Types_RawBytes = require("../Types.RawBytes/index.js");
var Types_TransactionMetadata = require("../Types.TransactionMetadata/index.js");
var toMetadataScriptHash = {
    toMetadata: (function () {
        var $30 = Metadata_ToMetadata.toMetadata(Types_RawBytes.toMetadataRawBytes);
        return function ($31) {
            return $30($foreign.scriptHashToBytes($31));
        };
    })()
};
var toMetadataEd25519KeyHash = {
    toMetadata: (function () {
        var $32 = Metadata_ToMetadata.toMetadata(Types_RawBytes.toMetadataRawBytes);
        return function ($33) {
            return $32($foreign.ed25519KeyHashToBytes($33));
        };
    })()
};
var toDataScriptHash = {
    toData: (function () {
        var $34 = ToData.toData(ToData.toDataByteArray);
        var $35 = Data_Newtype.unwrap();
        return function ($36) {
            return $34($35($foreign.scriptHashToBytes($36)));
        };
    })()
};
var toDataEd25519KeyHash = {
    toData: (function () {
        var $37 = ToData.toData(ToData.toDataByteArray);
        var $38 = Data_Newtype.unwrap();
        return function ($39) {
            return $37($38($foreign.ed25519KeyHashToBytes($39)));
        };
    })()
};
var showScriptHash = {
    show: function (edkh) {
        return "(ScriptHash " + (Types_RawBytes.rawBytesToHex($foreign.scriptHashToBytes(edkh)) + ")");
    }
};
var showEd25519KeyHash = {
    show: function (edkh) {
        return "(Ed25519KeyHash " + (Types_RawBytes.rawBytesToHex($foreign.ed25519KeyHashToBytes(edkh)) + ")");
    }
};
var eqScriptHash = {
    eq: Data_Function.on(Data_Eq.eq(Types_RawBytes.eqRawBytes))($foreign.scriptHashToBytes)
};
var ordScriptHash = {
    compare: Data_Function.on(Data_Ord.compare(Types_RawBytes.ordRawBytes))($foreign.scriptHashToBytes),
    Eq0: function () {
        return eqScriptHash;
    }
};
var eqEd25519KeyHash = {
    eq: Data_Function.on(Data_Eq.eq(Types_RawBytes.eqRawBytes))($foreign.ed25519KeyHashToBytes)
};
var ordEd25519KeyHash = {
    compare: Data_Function.on(Data_Ord.compare(Types_RawBytes.ordRawBytes))($foreign.ed25519KeyHashToBytes),
    Eq0: function () {
        return eqEd25519KeyHash;
    }
};
var encodeAesonScriptHash = {
    "encodeAeson'": function (sh) {
        return Aeson["encodeAeson'"](Types_RawBytes.encodeAesonRawBytes)($foreign.scriptHashToBytes(sh));
    }
};
var encodeAesonEd25519KeyHash = {
    "encodeAeson'": (function () {
        var $40 = Aeson["encodeAeson'"](Aeson.encodeAesonString);
        return function ($41) {
            return $40(Types_RawBytes.rawBytesToHex($foreign.ed25519KeyHashToBytes($41)));
        };
    })()
};
var scriptHashToBech32 = $foreign["_scriptHashToBech32Impl"](FfiHelpers.maybeFfiHelper);
var scriptHashFromBytes = $foreign["_scriptHashFromBytesImpl"](FfiHelpers.maybeFfiHelper);
var decodeAesonScriptHash = {
    decodeAeson: (function () {
        var $42 = Data_Maybe.maybe(Data_Either.Left.create(new Data_Argonaut_Decode_Error.TypeMismatch("Expected hex-encoded script hash")))(Data_Either.Right.create);
        var $43 = Aeson.caseAesonString(Data_Maybe.Nothing.value)(Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(Data_Maybe.Just.create)(Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(scriptHashFromBytes)(Types_RawBytes.hexToRawBytes)));
        return function ($44) {
            return $42($43($44));
        };
    })()
};
var fromDataScriptHash = {
    fromData: function (v) {
        if (v instanceof Types_PlutusData.Bytes) {
            return scriptHashFromBytes(Data_Newtype.wrap()(v.value0));
        };
        return Data_Maybe.Nothing.value;
    }
};
var fromMetadataScriptHash = {
    fromMetadata: function (v) {
        if (v instanceof Types_TransactionMetadata.Bytes) {
            return scriptHashFromBytes(Data_Newtype.wrap()(v.value0));
        };
        return Data_Maybe.Nothing.value;
    }
};
var scriptHashFromBech32 = $foreign["_scriptHashFromBech32Impl"](FfiHelpers.maybeFfiHelper);
var ed25519KeyHashToBech32 = $foreign["_ed25519KeyHashToBech32Impl"](FfiHelpers.maybeFfiHelper);
var ed25519KeyHashFromBytes = $foreign["_ed25519KeyHashFromBytesImpl"](FfiHelpers.maybeFfiHelper);
var decodeAesonEd25519KeyHash = {
    decodeAeson: Aeson.caseAesonString(Data_Either.Left.create(new Data_Argonaut_Decode_Error.TypeMismatch("Expected Plutus BuiltinByteString")))(Control_Bind.composeKleisliFlipped(Data_Either.bindEither)((function () {
        var $45 = Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("Invalid Ed25519KeyHash"));
        return function ($46) {
            return $45(ed25519KeyHashFromBytes($46));
        };
    })())((function () {
        var $47 = Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("Invalid ByteArray"));
        return function ($48) {
            return $47(Types_RawBytes.hexToRawBytes($48));
        };
    })()))
};
var fromDataEd25519KeyHash = {
    fromData: function (v) {
        if (v instanceof Types_PlutusData.Bytes) {
            return ed25519KeyHashFromBytes(Data_Newtype.wrap()(v.value0));
        };
        return Data_Maybe.Nothing.value;
    }
};
var fromMetadataEd25519KeyHas = {
    fromMetadata: function (v) {
        if (v instanceof Types_TransactionMetadata.Bytes) {
            return ed25519KeyHashFromBytes(Data_Newtype.wrap()(v.value0));
        };
        return Data_Maybe.Nothing.value;
    }
};
var ed25519KeyHashFromBech32 = $foreign["_ed25519KeyHashFromBech32Impl"](FfiHelpers.maybeFfiHelper);
module.exports = {
    ed25519KeyHashFromBytes: ed25519KeyHashFromBytes,
    ed25519KeyHashFromBech32: ed25519KeyHashFromBech32,
    ed25519KeyHashToBech32: ed25519KeyHashToBech32,
    scriptHashFromBytes: scriptHashFromBytes,
    scriptHashFromBech32: scriptHashFromBech32,
    scriptHashToBech32: scriptHashToBech32,
    eqEd25519KeyHash: eqEd25519KeyHash,
    ordEd25519KeyHash: ordEd25519KeyHash,
    showEd25519KeyHash: showEd25519KeyHash,
    toDataEd25519KeyHash: toDataEd25519KeyHash,
    fromDataEd25519KeyHash: fromDataEd25519KeyHash,
    toMetadataEd25519KeyHash: toMetadataEd25519KeyHash,
    fromMetadataEd25519KeyHas: fromMetadataEd25519KeyHas,
    decodeAesonEd25519KeyHash: decodeAesonEd25519KeyHash,
    encodeAesonEd25519KeyHash: encodeAesonEd25519KeyHash,
    eqScriptHash: eqScriptHash,
    ordScriptHash: ordScriptHash,
    showScriptHash: showScriptHash,
    toDataScriptHash: toDataScriptHash,
    fromDataScriptHash: fromDataScriptHash,
    toMetadataScriptHash: toMetadataScriptHash,
    fromMetadataScriptHash: fromMetadataScriptHash,
    decodeAesonScriptHash: decodeAesonScriptHash,
    encodeAesonScriptHash: encodeAesonScriptHash,
    ed25519KeyHashToBytes: $foreign.ed25519KeyHashToBytes,
    ed25519KeyHashToBech32Unsafe: $foreign.ed25519KeyHashToBech32Unsafe,
    scriptHashToBytes: $foreign.scriptHashToBytes,
    scriptHashToBech32Unsafe: $foreign.scriptHashToBech32Unsafe,
    nativeScriptHash: $foreign.nativeScriptHash
};
