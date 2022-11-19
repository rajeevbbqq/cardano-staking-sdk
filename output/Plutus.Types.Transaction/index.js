// Generated by purs version 0.14.5
"use strict";
var Cardano_Types_ScriptRef = require("../Cardano.Types.ScriptRef/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lens_Iso_Newtype = require("../Data.Lens.Iso.Newtype/index.js");
var Data_Lens_Record = require("../Data.Lens.Record/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var FromData = require("../FromData/index.js");
var Plutus_Types_Address = require("../Plutus.Types.Address/index.js");
var Plutus_Types_Value = require("../Plutus.Types.Value/index.js");
var Serialization_Hash = require("../Serialization.Hash/index.js");
var ToData = require("../ToData/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Types_OutputDatum = require("../Types.OutputDatum/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var TransactionOutput = function (x) {
    return x;
};
var TransactionOutputWithRefScript = function (x) {
    return x;
};
var toDataTransactionOutput = {
    toData: function (v) {
        return new Types_PlutusData.Constr(Data_Semiring.zero(Data_BigInt.semiringBigInt), [ ToData.toData(Plutus_Types_Address.toDataAddress)(v.address), ToData.toData(Plutus_Types_Value.toDataValue)(v.amount), ToData.toData(Types_OutputDatum.toDataOutputDatum)(v.datum), ToData.toData(ToData.toDataMaybe(Serialization_Hash.toDataScriptHash))(v.referenceScript) ]);
    }
};
var newtypeTransactionOutput_ = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeTransactionOutputW = {
    Coercible0: function () {
        return undefined;
    }
};
var toDataTransactionOutputWi = {
    toData: (function () {
        var $43 = ToData.toData(toDataTransactionOutput);
        var $44 = Data_Newtype.unwrap();
        return function ($45) {
            return $43((function (v) {
                return v.output;
            })($44($45)));
        };
    })()
};
var genericTransactionOutput_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showTransactionOutput = {
    show: Data_Show_Generic.genericShow(genericTransactionOutput_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "address";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "amount";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "datum";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "referenceScript";
        }
    })(Data_Show.showRecordFieldsNil)(Data_Maybe.showMaybe(Serialization_Hash.showScriptHash)))(Types_OutputDatum.showOutputDatum))(Plutus_Types_Value.showValue))(Plutus_Types_Address.showAddress))))({
        reflectSymbol: function () {
            return "TransactionOutput";
        }
    }))
};
var genericTransactionOutputW = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showTransactionOutputWith = {
    show: Data_Show_Generic.genericShow(genericTransactionOutputW)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "output";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "scriptRef";
        }
    })(Data_Show.showRecordFieldsNil)(Data_Maybe.showMaybe(Cardano_Types_ScriptRef.showScriptRef)))(showTransactionOutput))))({
        reflectSymbol: function () {
            return "TransactionOutputWithRefScript";
        }
    }))
};
var fromDataTransactionOutput = {
    fromData: function (v) {
        if (v instanceof Types_PlutusData.Constr && (v.value1.length === 4 && Data_Eq.eq(Data_BigInt.eqBigInt)(v.value0)(Data_Semiring.zero(Data_BigInt.semiringBigInt)))) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(TransactionOutput)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (v1) {
                return function (v2) {
                    return function (v3) {
                        return function (v4) {
                            return {
                                address: v1,
                                amount: v2,
                                datum: v3,
                                referenceScript: v4
                            };
                        };
                    };
                };
            })(FromData.fromData(Plutus_Types_Address.fromDataAddress)(v["value1"][0])))(FromData.fromData(Plutus_Types_Value.fromDataValue)(v["value1"][1])))(FromData.fromData(Types_OutputDatum.fromDataOutputDatum)(v["value1"][2])))(FromData.fromData(FromData.fromDataMaybe(Serialization_Hash.fromDataScriptHash))(v["value1"][3])));
        };
        return Data_Maybe.Nothing.value;
    }
};
var fromDataTransactionOutput1 = {
    fromData: (function () {
        var $46 = Data_Functor.map(Data_Maybe.functorMaybe)((function () {
            var $49 = Data_Newtype.wrap();
            return function ($50) {
                return $49((function (v) {
                    return {
                        output: v,
                        scriptRef: Data_Maybe.Nothing.value
                    };
                })($50));
            };
        })());
        var $47 = FromData.fromData(fromDataTransactionOutput);
        return function ($48) {
            return $46($47($48));
        };
    })()
};
var eqTransactionOutput = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "referenceScript";
    }
})(Data_Maybe.eqMaybe(Serialization_Hash.eqScriptHash)))()({
    reflectSymbol: function () {
        return "datum";
    }
})(Types_OutputDatum.eqOutputDatum))()({
    reflectSymbol: function () {
        return "amount";
    }
})(Plutus_Types_Value.eqValue))()({
    reflectSymbol: function () {
        return "address";
    }
})(Plutus_Types_Address.eqAddress));
var eqTransactionOutputWithRe = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "scriptRef";
    }
})(Data_Maybe.eqMaybe(Cardano_Types_ScriptRef.eqScriptRef)))()({
    reflectSymbol: function () {
        return "output";
    }
})(eqTransactionOutput));
var _scriptRef = function (dictStrong) {
    var $51 = Data_Lens_Iso_Newtype["_Newtype"]()()(dictStrong.Profunctor0());
    var $52 = Data_Lens_Record.prop({
        reflectSymbol: function () {
            return "scriptRef";
        }
    })()()(Type_Proxy["Proxy"].value)(dictStrong);
    return function ($53) {
        return $51($52($53));
    };
};
var _output = function (dictStrong) {
    var $54 = Data_Lens_Iso_Newtype["_Newtype"]()()(dictStrong.Profunctor0());
    var $55 = Data_Lens_Record.prop({
        reflectSymbol: function () {
            return "output";
        }
    })()()(Type_Proxy["Proxy"].value)(dictStrong);
    return function ($56) {
        return $54($55($56));
    };
};
var _datum = function (dictStrong) {
    var $57 = Data_Lens_Iso_Newtype["_Newtype"]()()(dictStrong.Profunctor0());
    var $58 = Data_Lens_Record.prop({
        reflectSymbol: function () {
            return "datum";
        }
    })()()(Type_Proxy["Proxy"].value)(dictStrong);
    return function ($59) {
        return $57($58($59));
    };
};
module.exports = {
    TransactionOutput: TransactionOutput,
    TransactionOutputWithRefScript: TransactionOutputWithRefScript,
    "_datum": _datum,
    "_output": _output,
    "_scriptRef": _scriptRef,
    genericTransactionOutput_: genericTransactionOutput_,
    newtypeTransactionOutput_: newtypeTransactionOutput_,
    eqTransactionOutput: eqTransactionOutput,
    showTransactionOutput: showTransactionOutput,
    fromDataTransactionOutput: fromDataTransactionOutput,
    toDataTransactionOutput: toDataTransactionOutput,
    genericTransactionOutputW: genericTransactionOutputW,
    newtypeTransactionOutputW: newtypeTransactionOutputW,
    eqTransactionOutputWithRe: eqTransactionOutputWithRe,
    showTransactionOutputWith: showTransactionOutputWith,
    fromDataTransactionOutput1: fromDataTransactionOutput1,
    toDataTransactionOutputWi: toDataTransactionOutputWi
};