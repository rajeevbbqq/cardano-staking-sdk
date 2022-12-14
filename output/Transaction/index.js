// Generated by purs version 0.14.5
"use strict";
var Cardano_Types_Transaction = require("../Cardano.Types.Transaction/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Deserialization_WitnessSet = require("../Deserialization.WitnessSet/index.js");
var Effect = require("../Effect/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Helpers = require("../Helpers/index.js");
var Serialization = require("../Serialization/index.js");
var Serialization_PlutusData = require("../Serialization.PlutusData/index.js");
var Serialization_PlutusScript = require("../Serialization.PlutusScript/index.js");
var Serialization_WitnessSet = require("../Serialization.WitnessSet/index.js");
var Untagged_Union = require("../Untagged.Union/index.js");
var ConvertWitnessesError = (function () {
    function ConvertWitnessesError() {

    };
    ConvertWitnessesError.value = new ConvertWitnessesError();
    return ConvertWitnessesError;
})();
var ConvertDatumError = (function () {
    function ConvertDatumError() {

    };
    ConvertDatumError.value = new ConvertDatumError();
    return ConvertDatumError;
})();
var genericModifyTxError_ = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return ConvertWitnessesError.value;
        };
        if (x instanceof Data_Generic_Rep.Inr) {
            return ConvertDatumError.value;
        };
        throw new Error("Failed pattern match at Transaction (line 48, column 1 - line 48, column 40): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof ConvertWitnessesError) {
            return new Data_Generic_Rep.Inl(Data_Generic_Rep.NoArguments.value);
        };
        if (x instanceof ConvertDatumError) {
            return new Data_Generic_Rep.Inr(Data_Generic_Rep.NoArguments.value);
        };
        throw new Error("Failed pattern match at Transaction (line 48, column 1 - line 48, column 40): " + [ x.constructor.name ]);
    }
};
var showModifyTxError = {
    show: Data_Show_Generic.genericShow(genericModifyTxError_)(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsNoArguments)({
        reflectSymbol: function () {
            return "ConvertWitnessesError";
        }
    }))(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsNoArguments)({
        reflectSymbol: function () {
            return "ConvertDatumError";
        }
    })))
};
var eqModifyTxError = {
    eq: function (x) {
        return function (y) {
            if (x instanceof ConvertWitnessesError && y instanceof ConvertWitnessesError) {
                return true;
            };
            if (x instanceof ConvertDatumError && y instanceof ConvertDatumError) {
                return true;
            };
            return false;
        };
    }
};
var updateTxWithWitnesses = function (v) {
    return function (ws) {
        return Helpers.liftEither(Control_Monad_Except_Trans.monadErrorExceptT(Effect.monadEffect))(Data_Either.Right.create(Data_Newtype.over()()(Cardano_Types_Transaction.Transaction)(function (v1) {
            return {
                witnessSet: Data_Semigroup.append(Cardano_Types_Transaction.semigroupTransactionWitne)(v.witnessSet)(ws),
                auxiliaryData: v1.auxiliaryData,
                body: v1.body,
                isValid: v1.isValid
            };
        })(v)));
    };
};
var setScriptDataHash = function (costModels) {
    return function (rs) {
        return function (ds) {
            return function (v) {
                var v1 = function (v2) {
                    if (Data_Boolean.otherwise) {
                        return function __do() {
                            var scriptDataHash = Data_Functor.map(Effect.functorEffect)((function () {
                                var $50 = Untagged_Union.asOneOf();
                                return function ($51) {
                                    return Cardano_Types_Transaction.ScriptDataHash(Serialization.toBytes($50($51)));
                                };
                            })())(Serialization.hashScriptData(costModels)(rs)(Data_Functor.map(Data_Functor.functorArray)(Data_Newtype.unwrap())(ds)))();
                            return Data_Newtype.over()()(Cardano_Types_Transaction.Transaction)(function (v3) {
                                return {
                                    body: Data_Newtype.over()()(Cardano_Types_Transaction.TxBody)(function (v4) {
                                        return {
                                            scriptDataHash: new Data_Maybe.Just(scriptDataHash),
                                            auxiliaryDataHash: v4.auxiliaryDataHash,
                                            certs: v4.certs,
                                            collateral: v4.collateral,
                                            collateralReturn: v4.collateralReturn,
                                            fee: v4.fee,
                                            inputs: v4.inputs,
                                            mint: v4.mint,
                                            networkId: v4.networkId,
                                            outputs: v4.outputs,
                                            referenceInputs: v4.referenceInputs,
                                            requiredSigners: v4.requiredSigners,
                                            totalCollateral: v4.totalCollateral,
                                            ttl: v4.ttl,
                                            update: v4.update,
                                            validityStartInterval: v4.validityStartInterval,
                                            withdrawals: v4.withdrawals
                                        };
                                    })(v.body),
                                    auxiliaryData: v3.auxiliaryData,
                                    isValid: v3.isValid,
                                    witnessSet: v3.witnessSet
                                };
                            })(v);
                        };
                    };
                    throw new Error("Failed pattern match at Transaction (line 56, column 1 - line 61, column 24): " + [ costModels.constructor.name, rs.constructor.name, ds.constructor.name, v.constructor.name ]);
                };
                var $36 = Data_Foldable["null"](Data_Foldable.foldableMaybe)((Data_Newtype.unwrap()(v.witnessSet)).plutusScripts);
                if ($36) {
                    var $37 = Data_Foldable["null"](Data_Foldable.foldableArray)(rs);
                    if ($37) {
                        var $38 = Data_Foldable["null"](Data_Foldable.foldableArray)(ds);
                        if ($38) {
                            return Control_Applicative.pure(Effect.applicativeEffect)(v);
                        };
                        return v1(true);
                    };
                    return v1(true);
                };
                return v1(true);
            };
        };
    };
};
var convertWitnessesWith = function (ws) {
    return function (act) {
        return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Effect_Class.liftEffect(Control_Monad_Except_Trans.monadEffectExceptT(Effect_Class.monadEffectEffect))(Serialization_WitnessSet.convertWitnessSet(ws)))(function (ws$prime) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Effect_Class.liftEffect(Control_Monad_Except_Trans.monadEffectExceptT(Effect_Class.monadEffectEffect))(act(ws$prime)))(function () {
                return Helpers.liftEither(Control_Monad_Except_Trans.monadErrorExceptT(Effect.monadEffect))(Data_Either.note(ConvertWitnessesError.value)(Deserialization_WitnessSet.convertWitnessSet(ws$prime)));
            });
        });
    };
};
var attachRedeemers = function (rs) {
    return function (v) {
        return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Effect_Class.liftEffect(Control_Monad_Except_Trans.monadEffectExceptT(Effect_Class.monadEffectEffect))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Serialization_WitnessSet.convertRedeemer)(rs)))(function (rs$prime) {
            return Control_Bind.bindFlipped(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(updateTxWithWitnesses(v))(convertWitnessesWith(v.witnessSet)(Serialization_WitnessSet.setRedeemers(rs$prime)));
        });
    };
};
var attachRedeemer = function (r) {
    var $52 = attachRedeemers(Data_Array.singleton(r));
    return function ($53) {
        return Control_Monad_Except_Trans.runExceptT($52($53));
    };
};
var attachPlutusScripts = function (ps) {
    return function (v) {
        var ps$prime = Data_Functor.map(Data_Functor.functorArray)(Serialization_PlutusScript.convertPlutusScript)(ps);
        return Control_Bind.bindFlipped(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(updateTxWithWitnesses(v))(convertWitnessesWith(v.witnessSet)(Serialization_WitnessSet.setPlutusScripts(ps$prime)));
    };
};
var attachPlutusScript = function (ps) {
    var $54 = attachPlutusScripts(Data_Array.singleton(ps));
    return function ($55) {
        return Control_Monad_Except_Trans.runExceptT($54($55));
    };
};
var attachNativeScript = function (ns) {
    return function (tx) {
        return Control_Monad_Except_Trans.runExceptT(updateTxWithWitnesses(tx)(Data_Newtype.over()()(Cardano_Types_Transaction.TransactionWitnessSet)(function (v) {
            return {
                nativeScripts: new Data_Maybe.Just([ ns ]),
                bootstraps: v.bootstraps,
                plutusData: v.plutusData,
                plutusScripts: v.plutusScripts,
                redeemers: v.redeemers,
                vkeys: v.vkeys
            };
        })(Data_Monoid.mempty(Cardano_Types_Transaction.monoidTransactionWitnessS))));
    };
};
var attachDatums = function (v) {
    return function (v1) {
        if (v.length === 0) {
            return Helpers.liftEither(Control_Monad_Except_Trans.monadErrorExceptT(Effect.monadEffect))(new Data_Either.Right(v1));
        };
        return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(Data_Traversable.traverse(Data_Traversable.traversableArray)(Control_Monad_Except_Trans.applicativeExceptT(Effect.monadEffect))((function () {
            var $56 = Helpers.liftEither(Control_Monad_Except_Trans.monadErrorExceptT(Effect.monadEffect));
            var $57 = Data_Either.note(ConvertDatumError.value);
            var $58 = Data_Newtype.unwrap();
            return function ($59) {
                return $56($57(Serialization_PlutusData.convertPlutusData($58($59))));
            };
        })())(v))(function (ds) {
            return Control_Bind.bindFlipped(Control_Monad_Except_Trans.bindExceptT(Effect.monadEffect))(updateTxWithWitnesses(v1))(convertWitnessesWith(v1.witnessSet)(Serialization_WitnessSet.setPlutusData(ds)));
        });
    };
};
var attachDatum = function (d) {
    var $60 = attachDatums(Data_Array.singleton(d));
    return function ($61) {
        return Control_Monad_Except_Trans.runExceptT($60($61));
    };
};
module.exports = {
    ConvertWitnessesError: ConvertWitnessesError,
    ConvertDatumError: ConvertDatumError,
    attachDatum: attachDatum,
    attachRedeemer: attachRedeemer,
    attachPlutusScript: attachPlutusScript,
    attachNativeScript: attachNativeScript,
    setScriptDataHash: setScriptDataHash,
    genericModifyTxError_: genericModifyTxError_,
    eqModifyTxError: eqModifyTxError,
    showModifyTxError: showModifyTxError
};
