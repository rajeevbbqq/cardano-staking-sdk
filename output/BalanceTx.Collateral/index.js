// Generated by purs version 0.14.5
"use strict";
var BalanceTx_Collateral_Select = require("../BalanceTx.Collateral.Select/index.js");
var BalanceTx_Error = require("../BalanceTx.Error/index.js");
var BalanceTx_UtxoMinAda = require("../BalanceTx.UtxoMinAda/index.js");
var Cardano_Types_Transaction = require("../Cardano.Types.Transaction/index.js");
var Cardano_Types_Value = require("../Cardano.Types.Value/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lens_Setter = require("../Data.Lens.Setter/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ord_Max = require("../Data.Ord.Max/index.js");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var QueryM = require("../QueryM/index.js");
var Types_BigNum = require("../Types.BigNum/index.js");
var Types_OutputDatum = require("../Types.OutputDatum/index.js");
var nonAdaAsset = (function () {
    var $13 = Data_Newtype.unwrap();
    var $14 = Data_Newtype.unwrap();
    return function ($15) {
        return Cardano_Types_Value.getNonAdaAsset((function (v) {
            return v.amount;
        })($13((function (v) {
            return v.output;
        })($14($15)))));
    };
})();
var addTxCollateral = function (collateral) {
    return function (transaction) {
        return Data_Lens_Setter.setJust((function () {
            var $16 = Cardano_Types_Transaction["_body"](Data_Profunctor_Strong.strongFn);
            var $17 = Cardano_Types_Transaction["_collateral"](Data_Profunctor_Strong.strongFn);
            return function ($18) {
                return $16($17($18));
            };
        })())(Data_Functor.map(Data_Functor.functorArray)((function () {
            var $19 = Data_Newtype.unwrap();
            return function ($20) {
                return (function (v) {
                    return v.input;
                })($19($20));
            };
        })())(collateral))(transaction);
    };
};
var adaValue = (function () {
    var $21 = Data_Newtype.unwrap();
    var $22 = Data_Newtype.unwrap();
    return function ($23) {
        return Cardano_Types_Value["valueToCoin'"]((function (v) {
            return v.amount;
        })($21((function (v) {
            return v.output;
        })($22($23)))));
    };
})();
var adaValue$prime = function (init) {
    var $24 = Data_Semiring.add(Data_BigInt.semiringBigInt)(init);
    return function ($25) {
        return $24(adaValue($25));
    };
};
var addTxCollateralReturn = function (collateral) {
    return function (transaction) {
        return function (ownAddress) {
            var setTxCollateralReturn = function (collAdaValue) {
                return function (collNonAdaAsset) {
                    var maxBigNumAdaValue = Data_Newtype.wrap()(Types_BigNum.toBigIntUnsafe(Types_BigNum.maxValue));
                    var collReturnOutputRec = {
                        address: ownAddress,
                        amount: Cardano_Types_Value.mkValue(maxBigNumAdaValue)(collNonAdaAsset),
                        datum: Types_OutputDatum.NoOutputDatum.value,
                        scriptRef: Data_Maybe.Nothing.value
                    };
                    return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(QueryM.monadQueryMExtendedAff))(Data_Functor.mapFlipped(Control_Monad_Except_Trans.functorExceptT(QueryM.functorQueryMExtended(Effect_Aff.functorAff)))(Control_Monad_Reader_Class.asks(Control_Monad_Except_Trans.monadAskExceptT(QueryM.monadAskQueryEnvQueryMExt))(function ($26) {
                        return (function (v) {
                            return v.pparams;
                        })((function (v) {
                            return v.runtime;
                        })($26));
                    }))((function () {
                        var $27 = Data_Newtype.unwrap();
                        return function ($28) {
                            return (function (v) {
                                return v.coinsPerUtxoUnit;
                            })($27($28));
                        };
                    })()))(function (coinsPerUtxoUnit) {
                        return Control_Bind.bind(Control_Monad_Except_Trans.bindExceptT(QueryM.monadQueryMExtendedAff))(Control_Monad_Except_Trans.ExceptT(Data_Functor.mapFlipped(QueryM.functorQueryMExtended(Effect_Aff.functorAff))(Effect_Class.liftEffect(QueryM.monadEffectQueryMExtended)(BalanceTx_UtxoMinAda.utxoMinAdaValue(coinsPerUtxoUnit)(Data_Newtype.wrap()(collReturnOutputRec))))(Data_Either.note(BalanceTx_Error.CollateralReturnMinAdaValueCalcError.value))))(function (minAdaValue) {
                            var collReturnAda = Data_Newtype.unwrap()(Data_Semigroup.append(Data_Ord_Max.semigroupMax(Data_BigInt.ordBigInt))(Data_Ring.sub(Data_BigInt.ringBigInt)(collAdaValue)(BalanceTx_Collateral_Select.minRequiredCollateral))(minAdaValue));
                            var collReturnOutput = Data_Newtype.wrap()({
                                amount: Cardano_Types_Value.mkValue(Data_Newtype.wrap()(collReturnAda))(collNonAdaAsset),
                                address: collReturnOutputRec.address,
                                datum: collReturnOutputRec.datum,
                                scriptRef: collReturnOutputRec.scriptRef
                            });
                            var totalCollateral = Data_Ring.sub(Data_BigInt.ringBigInt)(collAdaValue)(collReturnAda);
                            return Control_Monad_Except_Trans.except(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))((function () {
                                var v = Data_Ord.greaterThan(Data_BigInt.ordBigInt)(totalCollateral)(Data_Semiring.zero(Data_BigInt.semiringBigInt));
                                if (v) {
                                    return Data_Either.Right.create(Data_Lens_Setter.setJust((function () {
                                        var $29 = Cardano_Types_Transaction["_body"](Data_Profunctor_Strong.strongFn);
                                        var $30 = Cardano_Types_Transaction["_totalCollateral"](Data_Profunctor_Strong.strongFn);
                                        return function ($31) {
                                            return $29($30($31));
                                        };
                                    })())(Data_Newtype.wrap()(totalCollateral))(Data_Lens_Setter.setJust((function () {
                                        var $32 = Cardano_Types_Transaction["_body"](Data_Profunctor_Strong.strongFn);
                                        var $33 = Cardano_Types_Transaction["_collateralReturn"](Data_Profunctor_Strong.strongFn);
                                        return function ($34) {
                                            return $32($33($34));
                                        };
                                    })())(collReturnOutput)(transaction)));
                                };
                                if (!v) {
                                    return Data_Either.Left.create(new BalanceTx_Error.CollateralReturnError("Negative totalCollateral after covering min-utxo-ada requirement."));
                                };
                                throw new Error("Failed pattern match at BalanceTx.Collateral (line 118, column 7 - line 126, column 80): " + [ v.constructor.name ]);
                            })());
                        });
                    });
                };
            };
            var collNonAdaAsset = Data_Foldable.foldMap(Data_Foldable.foldableArray)(Cardano_Types_Value.monoidNonAdaAsset)(nonAdaAsset)(collateral);
            var collAdaValue = Data_Foldable.foldl(Data_Foldable.foldableArray)(adaValue$prime)(Data_Semiring.zero(Data_BigInt.semiringBigInt))(collateral);
            var v = Data_Ord.lessThanOrEq(Data_BigInt.ordBigInt)(collAdaValue)(BalanceTx_Collateral_Select.minRequiredCollateral) && Data_Eq.eq(Cardano_Types_Value.eqNonAdaAsset)(collNonAdaAsset)(Data_Monoid.mempty(Cardano_Types_Value.monoidNonAdaAsset));
            if (v) {
                return Control_Applicative.pure(Control_Monad_Except_Trans.applicativeExceptT(QueryM.monadQueryMExtendedAff))(transaction);
            };
            if (!v) {
                return setTxCollateralReturn(collAdaValue)(collNonAdaAsset);
            };
            throw new Error("Failed pattern match at BalanceTx.Collateral (line 71, column 5 - line 75, column 59): " + [ v.constructor.name ]);
        };
    };
};
module.exports = {
    addTxCollateral: addTxCollateral,
    addTxCollateralReturn: addTxCollateralReturn,
    minRequiredCollateral: BalanceTx_Collateral_Select.minRequiredCollateral
};
