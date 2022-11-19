// Generated by purs version 0.14.5
"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Metadata_FromMetadata = require("../Metadata.FromMetadata/index.js");
var Metadata_ToMetadata = require("../Metadata.ToMetadata/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Types_TransactionMetadata = require("../Types.TransactionMetadata/index.js");
var metadataLabel = function (dict) {
    return dict.metadataLabel;
};
var toGeneralTxMetadata = function (dictMetadataType) {
    var $3 = Data_Newtype.wrap();
    var $4 = Data_Map_Internal.singleton(metadataLabel(dictMetadataType)(Type_Proxy["Proxy"].value));
    var $5 = Metadata_ToMetadata.toMetadata(dictMetadataType.ToMetadata1());
    return function ($6) {
        return $3($4($5($6)));
    };
};
var fromGeneralTxMetadata = function (dictMetadataType) {
    return Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(Metadata_FromMetadata.fromMetadata(dictMetadataType.FromMetadata0()))((function () {
        var $7 = Data_Map_Internal.lookup(Types_TransactionMetadata.ordTransactionMetadatumLa)(metadataLabel(dictMetadataType)(Type_Proxy["Proxy"].value));
        var $8 = Data_Newtype.unwrap();
        return function ($9) {
            return $7($8($9));
        };
    })());
};
module.exports = {
    metadataLabel: metadataLabel,
    fromGeneralTxMetadata: fromGeneralTxMetadata,
    toGeneralTxMetadata: toGeneralTxMetadata
};