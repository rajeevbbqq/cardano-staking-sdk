// Generated by purs version 0.14.5
"use strict";
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var castableRecordRLNil = {};
var castableRecordRLConsOptional = function (dictCastableRecordRL) {
    return function (dictCastable) {
        return {};
    };
};
var castableRecordRLConsDirect = function (dictCastableRecordRL) {
    return {};
};
var castableRecordRLConsCastable = function (dictCastableRecordRL) {
    return function (dictCastable) {
        return {};
    };
};
var castableRecord = function (dictRowToList) {
    return function (dictRowToList1) {
        return function (dictCastableRecordRL) {
            return {};
        };
    };
};
var castableIntNumber = {};
var castableForeign = {};
var castableCharString = {};
var cast = function (dictCastable) {
    return Unsafe_Coerce.unsafeCoerce;
};
module.exports = {
    cast: cast,
    castableIntNumber: castableIntNumber,
    castableCharString: castableCharString,
    castableRecord: castableRecord,
    castableForeign: castableForeign,
    castableRecordRLNil: castableRecordRLNil,
    castableRecordRLConsDirect: castableRecordRLConsDirect,
    castableRecordRLConsCastable: castableRecordRLConsCastable,
    castableRecordRLConsOptional: castableRecordRLConsOptional
};
