// Generated by purs version 0.14.5
"use strict";
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Show = require("../Data.Show/index.js");
var Metadata_FromMetadata = require("../Metadata.FromMetadata/index.js");
var Metadata_ToMetadata = require("../Metadata.ToMetadata/index.js");
var Types_ByteArray = require("../Types.ByteArray/index.js");
var CborBytes = function (x) {
    return x;
};
var toMetadataCborBytes = Metadata_ToMetadata.toMetadataByteArray;
var semigroupCborBytes = Types_ByteArray.semigroupByteArray;
var ordCborBytes = Types_ByteArray.ordByteArray;
var newtypeCborBytes_ = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidCborBytes = Types_ByteArray.monoidByteArray;
var fromMetadataCborBytes = Metadata_FromMetadata.fromMetadataByteArray;
var eqCborBytes = Types_ByteArray.eqByteArray;
var encodeAesonCborBytes = Types_ByteArray.encodeAesonByteArray;
var decodeAesonCborBytes = Types_ByteArray.decodeAesonByteArray;
var arbitraryCborBytes = Types_ByteArray.arbitraryByteArray;
var rawBytesAsCborBytes = (function () {
    var $11 = Data_Newtype.wrap();
    var $12 = Data_Newtype.unwrap();
    return function ($13) {
        return $11($12($13));
    };
})();
var hexToCborBytesUnsafe = (function () {
    var $14 = Data_Newtype.wrap();
    return function ($15) {
        return $14(Types_ByteArray.hexToByteArrayUnsafe($15));
    };
})();
var hexToCborBytes = (function () {
    var $16 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap());
    return function ($17) {
        return $16(Types_ByteArray.hexToByteArray($17));
    };
})();
var cborBytesToIntArray = (function () {
    var $18 = Data_Newtype.unwrap();
    return function ($19) {
        return Types_ByteArray.byteArrayToIntArray($18($19));
    };
})();
var cborBytesToHex = (function () {
    var $20 = Data_Newtype.unwrap();
    return function ($21) {
        return Types_ByteArray.byteArrayToHex($20($21));
    };
})();
var showCborBytes = {
    show: function (rb) {
        return "(hexToCborBytesUnsafe " + (Data_Show.show(Data_Show.showString)(cborBytesToHex(rb)) + ")");
    }
};
var cborBytesToByteArray = Data_Newtype.unwrap();
var cborBytesFromIntArrayUnsafe = (function () {
    var $22 = Data_Newtype.wrap();
    return function ($23) {
        return $22(Types_ByteArray.byteArrayFromIntArrayUnsafe($23));
    };
})();
var cborBytesFromIntArray = (function () {
    var $24 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap());
    return function ($25) {
        return $24(Types_ByteArray.byteArrayFromIntArray($25));
    };
})();
var cborBytesFromByteArray = Data_Newtype.wrap();
var cborBytesFromAscii = (function () {
    var $26 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap());
    return function ($27) {
        return $26(Types_ByteArray.byteArrayFromAscii($27));
    };
})();
var byteLength = (function () {
    var $28 = Data_Newtype.unwrap();
    return function ($29) {
        return Types_ByteArray.byteLength($28($29));
    };
})();
module.exports = {
    CborBytes: CborBytes,
    cborBytesToByteArray: cborBytesToByteArray,
    cborBytesFromByteArray: cborBytesFromByteArray,
    cborBytesFromAscii: cborBytesFromAscii,
    cborBytesToIntArray: cborBytesToIntArray,
    cborBytesFromIntArray: cborBytesFromIntArray,
    cborBytesFromIntArrayUnsafe: cborBytesFromIntArrayUnsafe,
    cborBytesToHex: cborBytesToHex,
    byteLength: byteLength,
    hexToCborBytes: hexToCborBytes,
    hexToCborBytesUnsafe: hexToCborBytesUnsafe,
    rawBytesAsCborBytes: rawBytesAsCborBytes,
    showCborBytes: showCborBytes,
    newtypeCborBytes_: newtypeCborBytes_,
    eqCborBytes: eqCborBytes,
    ordCborBytes: ordCborBytes,
    semigroupCborBytes: semigroupCborBytes,
    monoidCborBytes: monoidCborBytes,
    encodeAesonCborBytes: encodeAesonCborBytes,
    decodeAesonCborBytes: decodeAesonCborBytes,
    arbitraryCborBytes: arbitraryCborBytes,
    toMetadataCborBytes: toMetadataCborBytes,
    fromMetadataCborBytes: fromMetadataCborBytes
};
