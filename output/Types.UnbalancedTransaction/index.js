// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Cardano_Types_Transaction = require("../Cardano.Types.Transaction/index.js");
var Cardano_Types_Value = require("../Cardano.Types.Value/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Lens_Lens = require("../Data.Lens.Lens/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Helpers = require("../Helpers/index.js");
var Serialization = require("../Serialization/index.js");
var Types_Datum = require("../Types.Datum/index.js");
var Types_Scripts = require("../Types.Scripts/index.js");
var Types_Transaction = require("../Types.Transaction/index.js");
var UnbalancedTx = function (x) {
    return x;
};
var ScriptDatum = (function () {
    function ScriptDatum(value0) {
        this.value0 = value0;
    };
    ScriptDatum.create = function (value0) {
        return new ScriptDatum(value0);
    };
    return ScriptDatum;
})();
var ScriptDatumHash = (function () {
    function ScriptDatumHash(value0) {
        this.value0 = value0;
    };
    ScriptDatumHash.create = function (value0) {
        return new ScriptDatumHash(value0);
    };
    return ScriptDatumHash;
})();
var ScriptOutput = function (x) {
    return x;
};
var PaymentPubKey = function (x) {
    return x;
};
var ordPaymentPubKey = Cardano_Types_Transaction.ordPublicKey;
var newtypeUnbalancedTx_ = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeScriptOutput_ = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypePaymentPubKey_ = {
    Coercible0: function () {
        return undefined;
    }
};
var genericUnbalancedTx_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showUnbalancedTx = {
    show: Data_Show_Generic.genericShow(genericUnbalancedTx_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "transaction";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "utxoIndex";
        }
    })(Data_Show.showRecordFieldsNil)(Data_Map_Internal.showMap(Types_Transaction.showTransactionInput)(Cardano_Types_Transaction.showTransactionOutput)))(Cardano_Types_Transaction.showTransaction))))({
        reflectSymbol: function () {
            return "UnbalancedTx";
        }
    }))
};
var genericScriptOutput_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var genericScriptDatum_ = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new ScriptDatum(x.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return new ScriptDatumHash(x.value0);
        };
        throw new Error("Failed pattern match at Types.UnbalancedTransaction (line 57, column 1 - line 57, column 38): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ScriptDatum) {
            return new Data_Generic_Rep.Inl(x.value0);
        };
        if (x instanceof ScriptDatumHash) {
            return new Data_Generic_Rep.Inr(x.value0);
        };
        throw new Error("Failed pattern match at Types.UnbalancedTransaction (line 57, column 1 - line 57, column 38): " + [ x.constructor.name ]);
    }
};
var showScriptDatum = {
    show: Data_Show_Generic.genericShow(genericScriptDatum_)(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Types_Datum.showDatum))({
        reflectSymbol: function () {
            return "ScriptDatum";
        }
    }))(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Types_Transaction.showDataHash))({
        reflectSymbol: function () {
            return "ScriptDatumHash";
        }
    })))
};
var showScriptOutput = {
    show: Data_Show_Generic.genericShow(genericScriptOutput_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "datum";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "validatorHash";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "value";
        }
    })(Data_Show.showRecordFieldsNil)(Cardano_Types_Value.showValue))(Types_Scripts.showValidatorHash))(showScriptDatum))))({
        reflectSymbol: function () {
            return "ScriptOutput";
        }
    }))
};
var genericPaymentPubKey_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showPaymentPubKey = {
    show: Data_Show_Generic.genericShow(genericPaymentPubKey_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Cardano_Types_Transaction.showPublicKey))({
        reflectSymbol: function () {
            return "PaymentPubKey";
        }
    }))
};
var eqUnbalancedTx = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "utxoIndex";
    }
})(Data_Map_Internal.eqMap(Types_Transaction.eqTransactionInput)(Cardano_Types_Transaction.eqTransactionOutput)))()({
    reflectSymbol: function () {
        return "transaction";
    }
})(Cardano_Types_Transaction.eqTransaction));
var eqScriptDatum = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ScriptDatum && y instanceof ScriptDatum) {
                return Data_Eq.eq(Types_Datum.eqDatum)(x.value0)(y.value0);
            };
            if (x instanceof ScriptDatumHash && y instanceof ScriptDatumHash) {
                return Data_Eq.eq(Types_Transaction.eqDataHash)(x.value0)(y.value0);
            };
            return false;
        };
    }
};
var eqScriptOutput = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "value";
    }
})(Cardano_Types_Value.eqValue))()({
    reflectSymbol: function () {
        return "validatorHash";
    }
})(Types_Scripts.eqValidatorHash))()({
    reflectSymbol: function () {
        return "datum";
    }
})(eqScriptDatum));
var eqPaymentPubKey = Cardano_Types_Transaction.eqPublicKey;
var encodeAesonUnbalancedTx = {
    "encodeAeson'": function (v) {
        return Aeson["encodeAeson'"](Aeson.encodeAesonRecord(Aeson.gEncodeAesonCons(Cardano_Types_Transaction.encodeAesonTransaction)(Aeson.gEncodeAesonCons(Aeson.encodeAesonAeson)(Aeson.gEncodeAesonNil)({
            reflectSymbol: function () {
                return "utxoIndex";
            }
        })())({
            reflectSymbol: function () {
                return "transaction";
            }
        })())())({
            utxoIndex: Helpers.encodeMap(Types_Transaction.encodeAesonTransactionInp)(Cardano_Types_Transaction.encodeAesonTransactionOut)(v.utxoIndex),
            transaction: v.transaction
        });
    }
};
var encodeAesonScriptDatum = {
    "encodeAeson'": function (v) {
        if (v instanceof ScriptDatum) {
            return Aeson["encodeAeson'"](Aeson.encodeAesonAeson)(Helpers["encodeTagged'"](Types_Datum.encodeAesonDatum)("ScriptDatum")(v.value0));
        };
        if (v instanceof ScriptDatumHash) {
            return Aeson["encodeAeson'"](Aeson.encodeAesonAeson)(Helpers["encodeTagged'"](Types_Transaction.encodeAesonDataHash)("ScriptDatumHash")(v.value0));
        };
        throw new Error("Failed pattern match at Types.UnbalancedTransaction (line 60, column 18 - line 62, column 74): " + [ v.constructor.name ]);
    }
};
var encodeAesonScriptOutput = Aeson.encodeAesonRecord(Aeson.gEncodeAesonCons(encodeAesonScriptDatum)(Aeson.gEncodeAesonCons(Types_Scripts.encodeAesonValidatorHash)(Aeson.gEncodeAesonCons(Cardano_Types_Value.encodeAesonValue)(Aeson.gEncodeAesonNil)({
    reflectSymbol: function () {
        return "value";
    }
})())({
    reflectSymbol: function () {
        return "validatorHash";
    }
})())({
    reflectSymbol: function () {
        return "datum";
    }
})())();
var payPubKeyVkey = function (v) {
    return v;
};
var payPubKeyRequiredSigner = function (v) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function ($73) {
        return Cardano_Types_Transaction.RequiredSigner(Serialization.publicKeyHash($73));
    })(Serialization.publicKeyFromBech32(v));
};
var emptyUnbalancedTx = {
    transaction: Data_Monoid.mempty(Cardano_Types_Transaction.monoidTransaction),
    utxoIndex: Data_Map_Internal.empty
};
var _utxoIndex = function (dictStrong) {
    return Data_Lens_Lens["lens'"](function (v) {
        return new Data_Tuple.Tuple(v.utxoIndex, function (utxoIx) {
            return {
                transaction: v.transaction,
                utxoIndex: utxoIx
            };
        });
    })(dictStrong);
};
var _transaction = function (dictStrong) {
    return Data_Lens_Lens["lens'"](function (v) {
        return new Data_Tuple.Tuple(v.transaction, function (tx) {
            return {
                transaction: tx,
                utxoIndex: v.utxoIndex
            };
        });
    })(dictStrong);
};
module.exports = {
    PaymentPubKey: PaymentPubKey,
    ScriptOutput: ScriptOutput,
    ScriptDatum: ScriptDatum,
    ScriptDatumHash: ScriptDatumHash,
    UnbalancedTx: UnbalancedTx,
    "_transaction": _transaction,
    "_utxoIndex": _utxoIndex,
    emptyUnbalancedTx: emptyUnbalancedTx,
    payPubKeyRequiredSigner: payPubKeyRequiredSigner,
    payPubKeyVkey: payPubKeyVkey,
    genericPaymentPubKey_: genericPaymentPubKey_,
    newtypePaymentPubKey_: newtypePaymentPubKey_,
    eqPaymentPubKey: eqPaymentPubKey,
    ordPaymentPubKey: ordPaymentPubKey,
    showPaymentPubKey: showPaymentPubKey,
    eqScriptDatum: eqScriptDatum,
    genericScriptDatum_: genericScriptDatum_,
    encodeAesonScriptDatum: encodeAesonScriptDatum,
    showScriptDatum: showScriptDatum,
    newtypeScriptOutput_: newtypeScriptOutput_,
    genericScriptOutput_: genericScriptOutput_,
    eqScriptOutput: eqScriptOutput,
    encodeAesonScriptOutput: encodeAesonScriptOutput,
    showScriptOutput: showScriptOutput,
    newtypeUnbalancedTx_: newtypeUnbalancedTx_,
    genericUnbalancedTx_: genericUnbalancedTx_,
    eqUnbalancedTx: eqUnbalancedTx,
    showUnbalancedTx: showUnbalancedTx,
    encodeAesonUnbalancedTx: encodeAesonUnbalancedTx
};