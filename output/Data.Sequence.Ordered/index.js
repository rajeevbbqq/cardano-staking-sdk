// Generated by purs version 0.14.5
"use strict";
var Data_FingerTree = require("../Data.FingerTree/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lazy = require("../Data.Lazy/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Sequence_Internal = require("../Data.Sequence.Internal/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var OrdSeq = function (x) {
    return x;
};
var toUnfoldableDescending = function (dictFunctor) {
    return function (dictUnfoldable) {
        return function (v) {
            return Data_Sequence_Internal.mapGetElem(dictFunctor)(Data_FingerTree.unfoldRight(dictUnfoldable)(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v));
        };
    };
};
var toUnfoldable = function (dictFunctor) {
    return function (dictUnfoldable) {
        return function (v) {
            return Data_Sequence_Internal.mapGetElem(dictFunctor)(Data_FingerTree.unfoldLeft(dictUnfoldable)(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v));
        };
    };
};
var split = function (dictPartial) {
    return function (f) {
        return function (x) {
            return Data_FingerTree.split(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)()(function (y) {
                return f(y)(new Data_Sequence_Internal.Key(x));
            });
        };
    };
};
var splitGEQ = function (dictOrd) {
    return split()(Data_Ord.greaterThanOrEq(Data_Sequence_Internal.ordKey(dictOrd)));
};
var splitGT = function (dictOrd) {
    return split()(Data_Ord.greaterThan(Data_Sequence_Internal.ordKey(dictOrd)));
};
var showOrdSeq = function (dictShow) {
    return {
        show: function (xs) {
            return "(OrdSeq.fromFoldable [" + (Data_Sequence_Internal.strJoin(dictShow)(",")(toUnfoldable(Data_Functor.functorArray)(Data_Unfoldable.unfoldableArray)(xs)) + "])");
        }
    };
};
var popLeast = function (dictOrd) {
    return function (v) {
        var v1 = Data_FingerTree.viewL(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v);
        if (v1 instanceof Data_FingerTree.NilL) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof Data_FingerTree.ConsL) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(Data_Sequence_Internal.getElem(v1.value0), Data_Lazy.force(v1.value1)));
        };
        throw new Error("Failed pattern match at Data.Sequence.Ordered (line 203, column 3 - line 205, column 66): " + [ v1.constructor.name ]);
    };
};
var popGreatest = function (dictOrd) {
    return function (v) {
        var v1 = Data_FingerTree.viewR(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v);
        if (v1 instanceof Data_FingerTree.NilR) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof Data_FingerTree.SnocR) {
            return new Data_Maybe.Just(new Data_Tuple.Tuple(Data_Sequence_Internal.getElem(v1.value1), Data_Lazy.force(v1.value0)));
        };
        throw new Error("Failed pattern match at Data.Sequence.Ordered (line 220, column 3 - line 222, column 66): " + [ v1.constructor.name ]);
    };
};
var partition = function (dictOrd) {
    return function (k) {
        return function (v) {
            var t = splitGEQ(dictOrd)(k)(v);
            var r = Data_Tuple.snd(t);
            var l = Data_Tuple.fst(t);
            return new Data_Tuple.Tuple(Data_Lazy.force(l), Data_Lazy.force(r));
        };
    };
};
var $$null = function (v) {
    if (v instanceof Data_FingerTree.Empty) {
        return true;
    };
    return false;
};
var merge = function (dictOrd) {
    return function (v) {
        return function (v1) {
            var go = function (as) {
                return function (bs) {
                    var v2 = Data_FingerTree.viewL(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(bs);
                    if (v2 instanceof Data_FingerTree.NilL) {
                        return as;
                    };
                    if (v2 instanceof Data_FingerTree.ConsL) {
                        var t = Data_FingerTree.split(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)()(function (c) {
                            return Data_Ord.greaterThan(Data_Sequence_Internal.ordKey(dictOrd))(c)(Data_Sequence_Internal.measure(Data_Sequence_Internal.measuredElemKey)(v2.value0));
                        })(as);
                        var r = Data_Lazy.force(Data_Tuple.snd(t));
                        var l = Data_Lazy.force(Data_Tuple.fst(t));
                        return Data_Semigroup.append(Data_FingerTree.semigroupFingerTree(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey))(l)(Data_FingerTree.cons(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v2.value0)(go(Data_Lazy.force(v2.value1))(r)));
                    };
                    throw new Error("Failed pattern match at Data.Sequence.Ordered (line 168, column 5 - line 174, column 47): " + [ v2.constructor.name ]);
                };
            };
            return go(v)(v1);
        };
    };
};
var semigroupOrdSeq = function (dictOrd) {
    return {
        append: merge(dictOrd)
    };
};
var mapOrdSeq = function (dictFunctor) {
    return Unsafe_Coerce.unsafeCoerce;
};
var least = function (dictOrd) {
    return function (v) {
        var v1 = Data_FingerTree.viewL(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v);
        if (v1 instanceof Data_FingerTree.NilL) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof Data_FingerTree.ConsL) {
            return new Data_Maybe.Just(Data_Sequence_Internal.getElem(v1.value0));
        };
        throw new Error("Failed pattern match at Data.Sequence.Ordered (line 195, column 3 - line 197, column 37): " + [ v1.constructor.name ]);
    };
};
var intersection = function (dictOrd) {
    return function (v) {
        return function (v1) {
            var go = function (as) {
                return function (bs) {
                    var v2 = new Data_Tuple.Tuple(Data_FingerTree.viewL(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(as), Data_FingerTree.viewL(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(bs));
                    if (v2.value0 instanceof Data_FingerTree.NilL) {
                        return Data_FingerTree.Empty.value;
                    };
                    if (v2.value1 instanceof Data_FingerTree.NilL) {
                        return Data_FingerTree.Empty.value;
                    };
                    if (v2.value0 instanceof Data_FingerTree.ConsL && v2.value1 instanceof Data_FingerTree.ConsL) {
                        var v3 = Data_Ord.compare(Data_Sequence_Internal.ordElem(dictOrd))(v2.value0.value0)(v2.value1.value0);
                        if (v3 instanceof Data_Ordering.LT) {
                            return go(Data_Lazy.force(v2.value0.value1))(bs);
                        };
                        if (v3 instanceof Data_Ordering.EQ) {
                            return Data_FingerTree.cons(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v2.value0.value0)(go(Data_Lazy.force(v2.value0.value1))(Data_Lazy.force(v2.value1.value1)));
                        };
                        if (v3 instanceof Data_Ordering.GT) {
                            return go(as)(Data_Lazy.force(v2.value1.value1));
                        };
                        throw new Error("Failed pattern match at Data.Sequence.Ordered (line 186, column 9 - line 189, column 34): " + [ v3.constructor.name ]);
                    };
                    throw new Error("Failed pattern match at Data.Sequence.Ordered (line 182, column 5 - line 189, column 34): " + [ v2.constructor.name ]);
                };
            };
            return go(v)(v1);
        };
    };
};
var insert = function (dictOrd) {
    return function (x) {
        return function (v) {
            var t = splitGEQ(dictOrd)(x)(v);
            var r = Data_Tuple.snd(t);
            var l = Data_Tuple.fst(t);
            return Data_FingerTree.append(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(Data_Lazy.force(l))(Data_FingerTree.cons(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(x)(Data_Lazy.force(r)));
        };
    };
};
var greatest = function (dictOrd) {
    return function (v) {
        var v1 = Data_FingerTree.viewR(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(v);
        if (v1 instanceof Data_FingerTree.NilR) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof Data_FingerTree.SnocR) {
            return new Data_Maybe.Just(Data_Sequence_Internal.getElem(v1.value1));
        };
        throw new Error("Failed pattern match at Data.Sequence.Ordered (line 211, column 3 - line 213, column 37): " + [ v1.constructor.name ]);
    };
};
var foldableOrdSeq = {
    foldr: function (f) {
        return function (z) {
            return function (v) {
                return Data_Foldable.foldr(Data_FingerTree.foldableFingerTree)(Data_Sequence_Internal.liftElem(f))(z)(v);
            };
        };
    },
    foldl: function (f) {
        return function (z) {
            return function (v) {
                return Data_Foldable.foldl(Data_FingerTree.foldableFingerTree)(Data_Sequence_Internal.lift2Elem(f))(z)(v);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_Foldable.foldMap(Data_FingerTree.foldableFingerTree)(dictMonoid)(Data_Sequence_Internal.liftElem(f))(v);
            };
        };
    }
};
var length = (function () {
    var $111 = Data_Newtype.unwrap();
    var $112 = Data_Foldable.foldMap(foldableOrdSeq)(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Function["const"](1));
    return function ($113) {
        return $111($112($113));
    };
})();
var eqOrdSeq = function (dictEq) {
    return {
        eq: function (v) {
            return function (v1) {
                return Data_FingerTree.eqFingerTree(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey)(Data_Sequence_Internal.eqElem(dictEq))(v)(v1);
            };
        }
    };
};
var empty = Data_FingerTree.Empty.value;
var fromFoldable = function (dictFoldable) {
    return function (dictOrd) {
        return Data_Foldable.foldr(dictFoldable)(insert(dictOrd))(empty);
    };
};
var sort = function (dictFunctor) {
    return function (dictFoldable) {
        return function (dictUnfoldable) {
            return function (dictOrd) {
                var $114 = toUnfoldable(dictFunctor)(dictUnfoldable);
                var $115 = fromFoldable(dictFoldable)(dictOrd);
                return function ($116) {
                    return $114($115($116));
                };
            };
        };
    };
};
var monoidOrdSeq = function (dictOrd) {
    return {
        mempty: empty,
        Semigroup0: function () {
            return semigroupOrdSeq(dictOrd);
        }
    };
};
var deleteAll = function (dictOrd) {
    return function (x) {
        return function (v) {
            var t = splitGEQ(dictOrd)(x)(v);
            var r = Data_Lazy.force(Data_Tuple.snd(t));
            var t$prime = splitGT(dictOrd)(x)(r);
            var r$prime = Data_Lazy.force(Data_Tuple.snd(t$prime));
            var l = Data_Lazy.force(Data_Tuple.fst(t));
            return Data_Semigroup.append(Data_FingerTree.semigroupFingerTree(Data_Sequence_Internal.monoidKey)(Data_Sequence_Internal.measuredElemKey))(l)(r$prime);
        };
    };
};
module.exports = {
    empty: empty,
    fromFoldable: fromFoldable,
    insert: insert,
    "null": $$null,
    length: length,
    least: least,
    greatest: greatest,
    popLeast: popLeast,
    popGreatest: popGreatest,
    partition: partition,
    merge: merge,
    intersection: intersection,
    deleteAll: deleteAll,
    toUnfoldable: toUnfoldable,
    toUnfoldableDescending: toUnfoldableDescending,
    sort: sort,
    eqOrdSeq: eqOrdSeq,
    showOrdSeq: showOrdSeq,
    semigroupOrdSeq: semigroupOrdSeq,
    monoidOrdSeq: monoidOrdSeq,
    foldableOrdSeq: foldableOrdSeq
};