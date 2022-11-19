// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Aeson_Decode = require("../Aeson.Decode/index.js");
var Aeson_Encode = require("../Aeson.Encode/index.js");
var Control_Lazy = require("../Control.Lazy/index.js");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_Contravariant = require("../Data.Functor.Contravariant/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Op = require("../Data.Op/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var FromData = require("../FromData/index.js");
var ToData = require("../ToData/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var Types_Transaction = require("../Types.Transaction/index.js");
var Datum = function (x) {
    return x;
};
var toDataDatum = ToData.toDataPlutusData;
var ordDatum = Types_PlutusData.ordPlutusData;
var newtypeDatum_ = {
    Coercible0: function () {
        return undefined;
    }
};
var genericDatum_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showDatum = {
    show: Data_Show_Generic.genericShow(genericDatum_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Types_PlutusData.showPlutusData))({
        reflectSymbol: function () {
            return "Datum";
        }
    }))
};
var fromDataDatum = FromData.fromDataPlutusData;
var eqDatum = Types_PlutusData.eqPlutusData;
var encodeAesonDatum = {
    "encodeAeson'": (function () {
        var $13 = Aeson["encodeAeson'"](Aeson.encodeAesonAeson);
        var $14 = Control_Lazy.defer(Control_Lazy.lazyFn)(Data_Function["const"](Aeson_Encode.encode(Data_Functor_Contravariant.cmap(Data_Op.contravariantOp)(Data_Newtype.unwrap())(Aeson_Encode.value(Types_PlutusData.encodeAesonPlutusData)))));
        return function ($15) {
            return $13($14($15));
        };
    })()
};
var decodeAesonDatum = {
    decodeAeson: Control_Lazy.defer(Control_Lazy.lazyFn)(Data_Function["const"](Aeson_Decode.decode(Data_Functor.map(Control_Monad_Reader_Trans.functorReaderT(Data_Either.functorEither))(Datum)(Aeson_Decode.value(Types_PlutusData.decodeAesonPlutusData)))))
};
var unitDatum = Datum(ToData.toData(ToData.toDataUnit)(Data_Unit.unit));
module.exports = {
    Datum: Datum,
    unitDatum: unitDatum,
    newtypeDatum_: newtypeDatum_,
    genericDatum_: genericDatum_,
    eqDatum: eqDatum,
    fromDataDatum: fromDataDatum,
    ordDatum: ordDatum,
    toDataDatum: toDataDatum,
    encodeAesonDatum: encodeAesonDatum,
    decodeAesonDatum: decodeAesonDatum,
    showDatum: showDatum,
    DataHash: Types_Transaction.DataHash
};