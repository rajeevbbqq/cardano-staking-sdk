// Generated by purs version 0.14.5
"use strict";
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Set = require("../Data.Set/index.js");
var Data_UInt = require("../Data.UInt/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Types_Transaction = require("../Types.Transaction/index.js");
var TxOutRefUnlockKeys = function (x) {
    return x;
};
var UsedTxOuts = function (x) {
    return x;
};
var LockTransactionError = (function () {
    function LockTransactionError() {

    };
    LockTransactionError.value = new LockTransactionError();
    return LockTransactionError;
})();
var newtypeUsedTxOuts_ = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeTxOutRefUnlockKeys = {
    Coercible0: function () {
        return undefined;
    }
};
var semigroupTxOutRefUnlockKe = {
    append: function (k1) {
        return function (k2) {
            return Data_Newtype.wrap()(Data_Map_Internal.unionWith(Types_Transaction.ordTransactionHash)(Data_Set.union(Data_UInt.uintOrd))(Data_Newtype.unwrap()(k1))(Data_Newtype.unwrap()(k2)));
        };
    }
};
var monoidTxOutRefUnlockKeys = {
    mempty: Data_Newtype.wrap()(Data_Map_Internal.empty),
    Semigroup0: function () {
        return semigroupTxOutRefUnlockKe;
    }
};
var unlockTxOutRefs = function (dictMonadAsk) {
    return function (dictMonadEffect) {
        return function (dictFoldable) {
            return function (txOutRefs$prime) {
                var updateCache = function (cache) {
                    return Data_Foldable.foldr(dictFoldable)(function (v) {
                        return Data_Map_Internal.update(Types_Transaction.ordTransactionHash)((function () {
                            var $48 = Data_Set["delete"](Data_UInt.uintOrd)(v.index);
                            return function ($49) {
                                return (function (s) {
                                    var $30 = Data_Set.isEmpty(s);
                                    if ($30) {
                                        return Data_Maybe.Nothing.value;
                                    };
                                    return new Data_Maybe.Just(s);
                                })($48($49));
                            };
                        })())(v.transactionId);
                    })(cache)(txOutRefs$prime);
                };
                return Control_Bind.bind((dictMonadAsk.Monad0()).Bind1())(Control_Monad_Reader_Class.ask(dictMonadAsk))((function () {
                    var $50 = Effect_Class.liftEffect(dictMonadEffect);
                    var $51 = Effect_Ref.modify_(updateCache);
                    var $52 = Data_Newtype.unwrap();
                    return function ($53) {
                        return $50($51($52($53)));
                    };
                })());
            };
        };
    };
};
var unlockTxOutKeys = function (dictMonadAsk) {
    return function (dictMonadEffect) {
        var flatten = function (v) {
            return Data_Functor.map(Data_Functor.functorArray)(function (v1) {
                return {
                    transactionId: v.value0,
                    index: v1
                };
            })(Data_Set.toUnfoldable(Data_Unfoldable.unfoldableArray)(v.value1));
        };
        var cacheToRefs = (function () {
            var $54 = Data_Array.concatMap(flatten);
            var $55 = Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray);
            return function ($56) {
                return $54($55($56));
            };
        })();
        var $57 = unlockTxOutRefs(dictMonadAsk)(dictMonadEffect)(Data_Foldable.foldableArray);
        var $58 = Data_Newtype.unwrap();
        return function ($59) {
            return $57(cacheToRefs($58($59)));
        };
    };
};
var txOutRefs = function (tx) {
    return Data_Set.map(Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()({
        reflectSymbol: function () {
            return "transactionId";
        }
    })(Types_Transaction.ordTransactionHash))()({
        reflectSymbol: function () {
            return "index";
        }
    })(Data_UInt.uintOrd)))(Data_Newtype.unwrap())((Data_Newtype.unwrap()((Data_Newtype.unwrap()(tx)).body)).inputs);
};
var unlockTransactionInputs = function (dictMonadAsk) {
    return function (dictMonadEffect) {
        var $60 = unlockTxOutRefs(dictMonadAsk)(dictMonadEffect)(Data_Set.foldableSet);
        return function ($61) {
            return $60(txOutRefs($61));
        };
    };
};
var newUsedTxOuts = function (dictMonadEffect) {
    return Data_Functor.map((((dictMonadEffect.Monad0()).Bind1()).Apply0()).Functor0())(UsedTxOuts)(Effect_Class.liftEffect(dictMonadEffect)(Effect_Ref["new"](Data_Map_Internal.empty)));
};
var cacheContains = function (cache) {
    return function (v) {
        return Data_Maybe.isJust(Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Map_Internal.lookup(Types_Transaction.ordTransactionHash)(v.transactionId)(cache))(function (indices) {
            return Control_Alternative.guard(Data_Maybe.alternativeMaybe)(Data_Set.member(Data_UInt.uintOrd)(v.index)(indices));
        }));
    };
};
var isTxOutRefUsed = function (dictMonadAsk) {
    return function (dictMonadEffect) {
        return function (ref) {
            return Control_Bind.bind((dictMonadAsk.Monad0()).Bind1())(Control_Bind.bindFlipped((dictMonadAsk.Monad0()).Bind1())((function () {
                var $62 = Effect_Class.liftEffect(dictMonadEffect);
                var $63 = Data_Newtype.unwrap();
                return function ($64) {
                    return $62(Effect_Ref.read($63($64)));
                };
            })())(Control_Monad_Reader_Class.ask(dictMonadAsk)))(function (cache) {
                return Control_Applicative.pure((dictMonadAsk.Monad0()).Applicative0())(cacheContains(cache)(ref));
            });
        };
    };
};
var lockRemainingTransactionInputs = function (dictMonadAsk) {
    return function (dictMonadError) {
        return function (dictMonadEffect) {
            return function (alreadyLocked) {
                return function (tx) {
                    var outRefs = Data_Set.filter(Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()({
                        reflectSymbol: function () {
                            return "transactionId";
                        }
                    })(Types_Transaction.ordTransactionHash))()({
                        reflectSymbol: function () {
                            return "index";
                        }
                    })(Data_UInt.uintOrd)))(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean))(cacheContains(Data_Newtype.unwrap()(alreadyLocked))))(txOutRefs(tx));
                    var isUnlocked = function (cache) {
                        return function (v) {
                            return Data_Maybe.maybe(true)(Data_HeytingAlgebra.not(Data_HeytingAlgebra.heytingAlgebraFunction(Data_HeytingAlgebra.heytingAlgebraBoolean))(Data_Set.member(Data_UInt.uintOrd)(v.index)))(Data_Map_Internal.lookup(Types_Transaction.ordTransactionHash)(v.transactionId)(cache));
                        };
                    };
                    var insertCache = function (v) {
                        return Data_Map_Internal.alter(Types_Transaction.ordTransactionHash)((function () {
                            var $65 = Data_Set.insert(Data_UInt.uintOrd)(v.index);
                            var $66 = Data_Maybe.fromMaybe(Data_Set.empty);
                            return function ($67) {
                                return Data_Maybe.Just.create($65($66($67)));
                            };
                        })())(v.transactionId);
                    };
                    var refsToTxOut = Data_Foldable.foldr(Data_Set.foldableSet)(insertCache)(Data_Map_Internal.empty);
                    var updateCache = function (cache) {
                        if (Data_Foldable.all(Data_Set.foldableSet)(Data_HeytingAlgebra.heytingAlgebraBoolean)(isUnlocked(cache))(outRefs)) {
                            return {
                                state: Data_Foldable.foldr(Data_Set.foldableSet)(insertCache)(cache)(outRefs),
                                value: true
                            };
                        };
                        if (Data_Boolean.otherwise) {
                            return {
                                state: cache,
                                value: false
                            };
                        };
                        throw new Error("Failed pattern match at Types.UsedTxOuts (line 95, column 5 - line 95, column 81): " + [ cache.constructor.name ]);
                    };
                    return Control_Bind.bind((dictMonadAsk.Monad0()).Bind1())(Data_Functor.map((((dictMonadAsk.Monad0()).Bind1()).Apply0()).Functor0())(Data_Newtype.unwrap())(Control_Monad_Reader_Class.ask(dictMonadAsk)))(function (cache) {
                        return Control_Bind.bind((dictMonadAsk.Monad0()).Bind1())(Effect_Class.liftEffect(dictMonadEffect)(Effect_Ref["modify'"](updateCache)(cache)))(function (success) {
                            return Control_Bind.discard(Control_Bind.discardUnit)((dictMonadAsk.Monad0()).Bind1())(Control_Applicative.unless((dictMonadAsk.Monad0()).Applicative0())(success)(Effect_Class.liftEffect(dictMonadEffect)(Effect_Exception["throw"]("Transaction inputs locked by another transaction"))))(function () {
                                return Control_Applicative.pure((dictMonadAsk.Monad0()).Applicative0())(Data_Semigroup.append(semigroupTxOutRefUnlockKe)(Data_Newtype.wrap()(refsToTxOut(outRefs)))(alreadyLocked));
                            });
                        });
                    });
                };
            };
        };
    };
};
var lockTransactionInputs = function (dictMonadAsk) {
    return function (dictMonadError) {
        return function (dictMonadEffect) {
            return lockRemainingTransactionInputs(dictMonadAsk)(dictMonadError)(dictMonadEffect)(Data_Monoid.mempty(monoidTxOutRefUnlockKeys));
        };
    };
};
var withLockedTransactionInputs = function (dictMonadAsk) {
    return function (dictMonadError) {
        return function (dictMonadEffect) {
            return function (t) {
                return function (f) {
                    return Control_Bind.bind((dictMonadAsk.Monad0()).Bind1())(lockTransactionInputs(dictMonadAsk)(dictMonadError)(dictMonadEffect)(t))(function (used) {
                        return Control_Monad_Error_Class.catchError(dictMonadError)(f)(function (e) {
                            return Control_Bind.discard(Control_Bind.discardUnit)((dictMonadAsk.Monad0()).Bind1())(unlockTxOutKeys(dictMonadAsk)(dictMonadEffect)(used))(function () {
                                return Control_Monad_Error_Class.throwError(dictMonadError.MonadThrow0())(e);
                            });
                        });
                    });
                };
            };
        };
    };
};
module.exports = {
    UsedTxOuts: UsedTxOuts,
    TxOutRefUnlockKeys: TxOutRefUnlockKeys,
    isTxOutRefUsed: isTxOutRefUsed,
    lockTransactionInputs: lockTransactionInputs,
    lockRemainingTransactionInputs: lockRemainingTransactionInputs,
    LockTransactionError: LockTransactionError,
    newUsedTxOuts: newUsedTxOuts,
    unlockTxOutRefs: unlockTxOutRefs,
    unlockTxOutKeys: unlockTxOutKeys,
    unlockTransactionInputs: unlockTransactionInputs,
    withLockedTransactionInputs: withLockedTransactionInputs,
    newtypeTxOutRefUnlockKeys: newtypeTxOutRefUnlockKeys,
    semigroupTxOutRefUnlockKe: semigroupTxOutRefUnlockKe,
    monoidTxOutRefUnlockKeys: monoidTxOutRefUnlockKeys,
    newtypeUsedTxOuts_: newtypeUsedTxOuts_
};
