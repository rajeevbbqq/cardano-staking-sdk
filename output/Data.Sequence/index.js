// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_FingerTree = require("../Data.FingerTree/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lazy = require("../Data.Lazy/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid_Additive = require("../Data.Monoid.Additive/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Sequence_Internal = require("../Data.Sequence.Internal/index.js");
var Data_Sequence_Ordered = require("../Data.Sequence.Ordered/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Seq = function (x) {
    return x;
};
var unsnoc = function (v) {
    var v1 = Data_FingerTree.viewR(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v);
    if (v1 instanceof Data_FingerTree.NilR) {
        return Data_Maybe.Nothing.value;
    };
    if (v1 instanceof Data_FingerTree.SnocR) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(Data_Lazy.force(v1.value0), Data_Sequence_Internal.getElem(v1.value1)));
    };
    throw new Error("Failed pattern match at Data.Sequence (line 227, column 3 - line 229, column 65): " + [ v1.constructor.name ]);
};
var unsafeIndex = function (dictPartial) {
    return function (i) {
        return function (v) {
            var v1 = Data_FingerTree.splitTree(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)()(function (n) {
                return i < Data_Newtype.unwrap()(n);
            })(0)(v);
            return Data_Sequence_Internal.getElem(v1.value1);
        };
    };
};
var unsafeAdjust = function (dictPartial) {
    return function (f) {
        return function (i) {
            return function (v) {
                var v1 = Data_FingerTree.splitTree(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)()(function (n) {
                    return i < Data_Newtype.unwrap()(n);
                })(0)(v);
                var l$prime = Data_FingerTree.snoc(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(Data_Lazy.force(v1.value0))(f(v1.value1));
                return Data_FingerTree.append(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(l$prime)(Data_Lazy.force(v1.value2));
            };
        };
    };
};
var uncons = function (v) {
    var v1 = Data_FingerTree.viewL(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v);
    if (v1 instanceof Data_FingerTree.NilL) {
        return Data_Maybe.Nothing.value;
    };
    if (v1 instanceof Data_FingerTree.ConsL) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(Data_Sequence_Internal.getElem(v1.value0), Data_Lazy.force(v1.value1)));
    };
    throw new Error("Failed pattern match at Data.Sequence (line 218, column 3 - line 220, column 65): " + [ v1.constructor.name ]);
};
var toUnfoldable = function (dictFunctor) {
    return function (dictUnfoldable) {
        return function (v) {
            return Data_Sequence_Internal.mapGetElem(dictFunctor)(Data_FingerTree.unfoldLeft(dictUnfoldable)(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v));
        };
    };
};
var splitAt$prime = function (i) {
    return function (v) {
        var tuple = Data_FingerTree.split(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)()(function (n) {
            return i < Data_Newtype.unwrap()(n);
        })(v);
        var seqify = function (dictFunctor) {
            return Unsafe_Coerce.unsafeCoerce;
        };
        return seqify(Data_Lazy.functorLazy)(tuple);
    };
};
var take = function (i) {
    var $119 = splitAt$prime(i);
    return function ($120) {
        return Data_Lazy.force(Data_Tuple.fst($119($120)));
    };
};
var splitAt = function (i) {
    return function (xs) {
        var tuple = splitAt$prime(i)(xs);
        var forceBoth = Data_Profunctor_Strong.splitStrong(Control_Category.categoryFn)(Data_Profunctor_Strong.strongFn)(Data_Lazy.force)(Data_Lazy.force);
        return forceBoth(tuple);
    };
};
var snoc = function (v) {
    return function (x) {
        return Data_FingerTree.snoc(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v)(x);
    };
};
var showSeq = function (dictShow) {
    return {
        show: function (xs) {
            return "(Seq.fromFoldable [" + (Data_Sequence_Internal.strJoin(dictShow)(",")(toUnfoldable(Data_Functor.functorArray)(Data_Unfoldable.unfoldableArray)(xs)) + "])");
        }
    };
};
var $$null = function (v) {
    if (v instanceof Data_FingerTree.Empty) {
        return true;
    };
    return false;
};
var mapSeq = function (dictFunctor) {
    return Unsafe_Coerce.unsafeCoerce;
};
var tail = function (v) {
    return mapSeq(Data_Maybe.functorMaybe)(Data_FingerTree.tail(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v));
};
var map = function (f) {
    return function (v) {
        return Data_Functor.map(Data_FingerTree.functorFingerTree)(f)(v);
    };
};
var length = function (v) {
    return Data_Newtype.un()(Data_Monoid_Additive.Additive)(Data_Sequence_Internal.measure(Data_FingerTree.measuredFingerTree(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem))(v));
};
var last = function (v) {
    return Data_Sequence_Internal.mapGetElem(Data_Maybe.functorMaybe)(Data_FingerTree.last(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v));
};
var init = function (v) {
    return mapSeq(Data_Maybe.functorMaybe)(Data_FingerTree.init(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v));
};
var inBounds = function (i) {
    return function (seq) {
        return 0 <= i && i < length(seq);
    };
};
var index = function (i) {
    return function (xs) {
        var $85 = inBounds(i)(xs);
        if ($85) {
            return Data_Maybe.Just.create(unsafeIndex()(i)(xs));
        };
        return Data_Maybe.Nothing.value;
    };
};
var head = function (v) {
    return Data_Sequence_Internal.mapGetElem(Data_Maybe.functorMaybe)(Data_FingerTree.head(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v));
};
var functorSeq = {
    map: map
};
var fullyForce = function (v) {
    return Data_FingerTree.fullyForce(v);
};
var foldableSeq = {
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
var traversableSeq = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return mapSeq((dictApplicative.Apply0()).Functor0())(Data_Traversable.traverse(Data_FingerTree.traversableFingerTree)(dictApplicative)(Data_Traversable.traverse(Data_Sequence_Internal.traversableElem)(dictApplicative)(f))(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return Data_Traversable.traverse(traversableSeq)(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
    },
    Functor0: function () {
        return functorSeq;
    },
    Foldable1: function () {
        return foldableSeq;
    }
};
var filter = function (p) {
    return function (v) {
        return Data_FingerTree.filter(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(p)(v);
    };
};
var eqSeq = function (dictEq) {
    return {
        eq: function (v) {
            return function (v1) {
                return Data_FingerTree.eqFingerTree(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(Data_Sequence_Internal.eqElem(dictEq))(v)(v1);
            };
        }
    };
};
var ordSeq = function (dictOrd) {
    return {
        compare: function (v) {
            return function (v1) {
                return Data_FingerTree.compareFingerTree(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(Data_Sequence_Internal.ordElem(dictOrd))(v)(v1);
            };
        },
        Eq0: function () {
            return eqSeq(dictOrd.Eq0());
        }
    };
};
var empty = Data_FingerTree.Empty.value;
var unfoldable1Seq = {
    unfoldr1: function (f) {
        return function (xs) {
            var go = function ($copy_source) {
                return function ($copy_memo) {
                    var $tco_var_source = $copy_source;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(source, memo) {
                        var v = f(source);
                        if (v.value1 instanceof Data_Maybe.Nothing) {
                            $tco_done = true;
                            return snoc(memo)(v.value0);
                        };
                        if (v.value1 instanceof Data_Maybe.Just) {
                            $tco_var_source = v.value1.value0;
                            $copy_memo = snoc(memo)(v.value0);
                            return;
                        };
                        throw new Error("Failed pattern match at Data.Sequence (line 128, column 9 - line 133, column 32): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_source, $copy_memo);
                    };
                    return $tco_result;
                };
            };
            return go(xs)(empty);
        };
    }
};
var unfoldableSeq = {
    unfoldr: function (f) {
        return function (xs) {
            var go = function ($copy_source) {
                return function ($copy_memo) {
                    var $tco_var_source = $copy_source;
                    var $tco_done = false;
                    var $tco_result;
                    function $tco_loop(source, memo) {
                        var v = f(source);
                        if (v instanceof Data_Maybe.Nothing) {
                            $tco_done = true;
                            return memo;
                        };
                        if (v instanceof Data_Maybe.Just) {
                            $tco_var_source = v.value0.value1;
                            $copy_memo = snoc(memo)(v.value0.value0);
                            return;
                        };
                        throw new Error("Failed pattern match at Data.Sequence (line 139, column 7 - line 144, column 30): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_source, $copy_memo);
                    };
                    return $tco_result;
                };
            };
            return go(xs)(empty);
        };
    },
    Unfoldable10: function () {
        return unfoldable1Seq;
    }
};
var sort = function (dictOrd) {
    return Data_Sequence_Ordered.sort(functorSeq)(foldableSeq)(unfoldableSeq)(dictOrd);
};
var drop = function (i) {
    var $121 = splitAt$prime(i);
    return function ($122) {
        return Data_Lazy.force(Data_Tuple.snd($121($122)));
    };
};
var cons = function (x) {
    return function (v) {
        return Data_FingerTree.cons(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(x)(v);
    };
};
var fromFoldable = function (dictFoldable) {
    return Data_Foldable.foldr(dictFoldable)(cons)(empty);
};
var singleton = function (x) {
    return cons(x)(empty);
};
var append = function (v) {
    return function (v1) {
        return Data_FingerTree.append(Data_Monoid_Additive.monoidAdditive(Data_Semiring.semiringInt))(Data_Sequence_Internal.measuredElem)(v)(v1);
    };
};
var concat = Data_Foldable.foldr(foldableSeq)(append)(empty);
var concatMap = function (f) {
    var $123 = map(f);
    return function ($124) {
        return concat($123($124));
    };
};
var monadSeq = {
    Applicative0: function () {
        return applicativeSeq;
    },
    Bind1: function () {
        return bindSeq;
    }
};
var bindSeq = {
    bind: Data_Function.flip(concatMap),
    Apply0: function () {
        return applySeq;
    }
};
var applySeq = {
    apply: Control_Monad.ap(monadSeq),
    Functor0: function () {
        return functorSeq;
    }
};
var applicativeSeq = {
    pure: singleton,
    Apply0: function () {
        return applySeq;
    }
};
var semigroupSeq = {
    append: append
};
var monoidSeq = {
    mempty: empty,
    Semigroup0: function () {
        return semigroupSeq;
    }
};
var altSeq = {
    alt: append,
    Functor0: function () {
        return functorSeq;
    }
};
var plusSeq = {
    empty: empty,
    Alt0: function () {
        return altSeq;
    }
};
var alternativeSeq = {
    Applicative0: function () {
        return applicativeSeq;
    },
    Plus1: function () {
        return plusSeq;
    }
};
var monadPlusSeq = {
    Monad0: function () {
        return monadSeq;
    },
    Alternative1: function () {
        return alternativeSeq;
    }
};
var monadZeroSeq = {
    Monad0: function () {
        return monadSeq;
    },
    Alternative1: function () {
        return alternativeSeq;
    },
    MonadZeroIsDeprecated2: function () {
        return undefined;
    }
};
var adjust = function (f) {
    return function (i) {
        return function (xs) {
            var $118 = inBounds(i)(xs);
            if ($118) {
                return unsafeAdjust()(f)(i)(xs);
            };
            return xs;
        };
    };
};
var replace = function (x) {
    return adjust(Data_Function["const"](x));
};
module.exports = {
    empty: empty,
    singleton: singleton,
    cons: cons,
    snoc: snoc,
    append: append,
    map: map,
    concat: concat,
    concatMap: concatMap,
    fromFoldable: fromFoldable,
    length: length,
    "null": $$null,
    inBounds: inBounds,
    uncons: uncons,
    unsnoc: unsnoc,
    head: head,
    tail: tail,
    init: init,
    last: last,
    toUnfoldable: toUnfoldable,
    splitAt: splitAt,
    take: take,
    drop: drop,
    filter: filter,
    sort: sort,
    index: index,
    adjust: adjust,
    replace: replace,
    fullyForce: fullyForce,
    ordSeq: ordSeq,
    eqSeq: eqSeq,
    showSeq: showSeq,
    semigroupSeq: semigroupSeq,
    monoidSeq: monoidSeq,
    foldableSeq: foldableSeq,
    traversableSeq: traversableSeq,
    unfoldable1Seq: unfoldable1Seq,
    unfoldableSeq: unfoldableSeq,
    functorSeq: functorSeq,
    applySeq: applySeq,
    applicativeSeq: applicativeSeq,
    bindSeq: bindSeq,
    monadSeq: monadSeq,
    altSeq: altSeq,
    plusSeq: plusSeq,
    alternativeSeq: alternativeSeq,
    monadPlusSeq: monadPlusSeq,
    monadZeroSeq: monadZeroSeq
};
