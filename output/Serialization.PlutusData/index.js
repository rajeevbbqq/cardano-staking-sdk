// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var Types_BigNum = require("../Types.BigNum/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var convertBigInt = function (n) {
    return $foreign["_bigIntFromString"](FfiHelpers.maybeFfiHelper)(Data_BigInt.toString(n));
};
var convertPlutusInteger = function (n) {
    return Data_Functor.map(Data_Maybe.functorMaybe)($foreign["_mkPlutusData_integer"])(convertBigInt(n));
};
var convertPlutusMap = function (mp) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(mp)(function (v) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(convertPlutusData(v.value0))(function (k$prime) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(convertPlutusData(v.value1))(function (v$prime) {
                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(k$prime, v$prime));
            });
        });
    }))(function (entries) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)($foreign["_mkPlutusData_map"]($foreign["_packMap"](Data_Tuple.fst)(Data_Tuple.snd)(entries)));
    });
};
var convertPlutusList = function (x) {
    return Data_Functor.map(Data_Maybe.functorMaybe)((function () {
        var $12 = $foreign["_packPlutusList"](FfiHelpers.containerHelper);
        return function ($13) {
            return $foreign["_mkPlutusData_list"]($12($13));
        };
    })())(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertPlutusData)(x));
};
var convertPlutusData = function (v) {
    if (v instanceof Types_PlutusData.Constr) {
        return convertConstr(v.value0)(v.value1);
    };
    if (v instanceof Types_PlutusData["Map"]) {
        return convertPlutusMap(v.value0);
    };
    if (v instanceof Types_PlutusData.List) {
        return convertPlutusList(v.value0);
    };
    if (v instanceof Types_PlutusData.Integer) {
        return convertPlutusInteger(v.value0);
    };
    if (v instanceof Types_PlutusData.Bytes) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)($foreign["_mkPlutusData_bytes"](v.value0));
    };
    throw new Error("Failed pattern match at Serialization.PlutusData (line 32, column 21 - line 37, column 44): " + [ v.constructor.name ]);
};
var convertConstr = function (alt) {
    return function (list) {
        return Data_Functor.map(Data_Maybe.functorMaybe)($foreign["_mkPlutusData_constr"])(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)($foreign["_mkConstrPlutusData"])(Types_BigNum.fromBigInt(alt)))(Data_Functor.map(Data_Maybe.functorMaybe)($foreign["_packPlutusList"](FfiHelpers.containerHelper))(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(list)(convertPlutusData))));
    };
};
var packPlutusList = (function () {
    var $14 = Data_Functor.map(Data_Maybe.functorMaybe)($foreign["_packPlutusList"](FfiHelpers.containerHelper));
    var $15 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertPlutusData);
    return function ($16) {
        return $14($15($16));
    };
})();
module.exports = {
    convertPlutusData: convertPlutusData,
    packPlutusList: packPlutusList
};