// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Deserialization_BigInt = require("../Deserialization.BigInt/index.js");
var Deserialization_FromBytes = require("../Deserialization.FromBytes/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var FromData = require("../FromData/index.js");
var Types_BigNum = require("../Types.BigNum/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var convertPlutusInteger = function (pd) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(Types_PlutusData.Integer.create)(Control_Bind.bind(Data_Maybe.bindMaybe)($foreign["_PlutusData_integer"](FfiHelpers.maybeFfiHelper)(pd))(Deserialization_BigInt.convertBigInt));
};
var convertPlutusBytes = function (pd) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(Types_PlutusData.Bytes.create)($foreign["_PlutusData_bytes"](FfiHelpers.maybeFfiHelper)(pd));
};
var convertPlutusMap = function (pd) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)(Control_Bind.bind(Data_Maybe.bindMaybe)($foreign["_PlutusData_map"](FfiHelpers.maybeFfiHelper)(pd))((function () {
        var $5 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function (v) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(convertPlutusData(v.value0))(function (k$prime) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(convertPlutusData(v.value1))(function (v$prime) {
                    return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(k$prime, v$prime));
                });
            });
        });
        var $6 = $foreign["_unpackPlutusMap"](FfiHelpers.containerHelper)(Data_Tuple.Tuple.create);
        return function ($7) {
            return $5($6($7));
        };
    })()))(function (entries) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Types_PlutusData["Map"](entries));
    });
};
var convertPlutusList = function (pd) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(Types_PlutusData.List.create)(Control_Bind.bind(Data_Maybe.bindMaybe)($foreign["_PlutusData_list"](FfiHelpers.maybeFfiHelper)(pd))((function () {
        var $8 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertPlutusData);
        var $9 = $foreign["_unpackPlutusList"](FfiHelpers.containerHelper);
        return function ($10) {
            return $8($9($10));
        };
    })()));
};
var convertPlutusData = function (pd) {
    return Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(convertPlutusConstr(pd))(convertPlutusMap(pd)))(convertPlutusList(pd)))(convertPlutusInteger(pd)))(convertPlutusBytes(pd));
};
var convertPlutusConstr = function (pd) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)($foreign["_PlutusData_constr"](FfiHelpers.maybeFfiHelper)(pd))(function (constr) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertPlutusData)($foreign["_unpackPlutusList"](FfiHelpers.containerHelper)($foreign["_ConstrPlutusData_data"](constr))))(function (data$prime) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Types_BigNum.toBigInt($foreign["_ConstrPlutusData_alternative"](constr)))(function (alt) {
                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Types_PlutusData.Constr(alt, data$prime));
            });
        });
    });
};
var deserializeData = function (dictFromData) {
    var $11 = Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(FromData.fromData(dictFromData))(Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(convertPlutusData)(Deserialization_FromBytes.fromBytes(Deserialization_FromBytes.fromBytesPlutusData)));
    var $12 = Data_Newtype.unwrap();
    return function ($13) {
        return $11($12($13));
    };
};
module.exports = {
    convertPlutusData: convertPlutusData,
    deserializeData: deserializeData
};