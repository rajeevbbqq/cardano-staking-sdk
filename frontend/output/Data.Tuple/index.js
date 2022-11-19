// Generated by purs version 0.14.5
"use strict";
var Control_Lazy = require("../Control.Lazy/index.js");
var Data_Bounded = require("../Data.Bounded/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Tuple = (function () {
    function Tuple(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Tuple.create = function (value0) {
        return function (value1) {
            return new Tuple(value0, value1);
        };
    };
    return Tuple;
})();
var uncurry = function (f) {
    return function (v) {
        return f(v.value0)(v.value1);
    };
};
var swap = function (v) {
    return new Tuple(v.value1, v.value0);
};
var snd = function (v) {
    return v.value1;
};
var showTuple = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (v) {
                return "(Tuple " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(dictShow1)(v.value1) + ")")));
            }
        };
    };
};
var semiringTuple = function (dictSemiring) {
    return function (dictSemiring1) {
        return {
            add: function (v) {
                return function (v1) {
                    return new Tuple(Data_Semiring.add(dictSemiring)(v.value0)(v1.value0), Data_Semiring.add(dictSemiring1)(v.value1)(v1.value1));
                };
            },
            one: new Tuple(Data_Semiring.one(dictSemiring), Data_Semiring.one(dictSemiring1)),
            mul: function (v) {
                return function (v1) {
                    return new Tuple(Data_Semiring.mul(dictSemiring)(v.value0)(v1.value0), Data_Semiring.mul(dictSemiring1)(v.value1)(v1.value1));
                };
            },
            zero: new Tuple(Data_Semiring.zero(dictSemiring), Data_Semiring.zero(dictSemiring1))
        };
    };
};
var semigroupoidTuple = {
    compose: function (v) {
        return function (v1) {
            return new Tuple(v1.value0, v.value1);
        };
    }
};
var semigroupTuple = function (dictSemigroup) {
    return function (dictSemigroup1) {
        return {
            append: function (v) {
                return function (v1) {
                    return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), Data_Semigroup.append(dictSemigroup1)(v.value1)(v1.value1));
                };
            }
        };
    };
};
var ringTuple = function (dictRing) {
    return function (dictRing1) {
        return {
            sub: function (v) {
                return function (v1) {
                    return new Tuple(Data_Ring.sub(dictRing)(v.value0)(v1.value0), Data_Ring.sub(dictRing1)(v.value1)(v1.value1));
                };
            },
            Semiring0: function () {
                return semiringTuple(dictRing.Semiring0())(dictRing1.Semiring0());
            }
        };
    };
};
var monoidTuple = function (dictMonoid) {
    return function (dictMonoid1) {
        return {
            mempty: new Tuple(Data_Monoid.mempty(dictMonoid), Data_Monoid.mempty(dictMonoid1)),
            Semigroup0: function () {
                return semigroupTuple(dictMonoid.Semigroup0())(dictMonoid1.Semigroup0());
            }
        };
    };
};
var heytingAlgebraTuple = function (dictHeytingAlgebra) {
    return function (dictHeytingAlgebra1) {
        return {
            tt: new Tuple(Data_HeytingAlgebra.tt(dictHeytingAlgebra), Data_HeytingAlgebra.tt(dictHeytingAlgebra1)),
            ff: new Tuple(Data_HeytingAlgebra.ff(dictHeytingAlgebra), Data_HeytingAlgebra.ff(dictHeytingAlgebra1)),
            implies: function (v) {
                return function (v1) {
                    return new Tuple(Data_HeytingAlgebra.implies(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.implies(dictHeytingAlgebra1)(v.value1)(v1.value1));
                };
            },
            conj: function (v) {
                return function (v1) {
                    return new Tuple(Data_HeytingAlgebra.conj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.conj(dictHeytingAlgebra1)(v.value1)(v1.value1));
                };
            },
            disj: function (v) {
                return function (v1) {
                    return new Tuple(Data_HeytingAlgebra.disj(dictHeytingAlgebra)(v.value0)(v1.value0), Data_HeytingAlgebra.disj(dictHeytingAlgebra1)(v.value1)(v1.value1));
                };
            },
            not: function (v) {
                return new Tuple(Data_HeytingAlgebra.not(dictHeytingAlgebra)(v.value0), Data_HeytingAlgebra.not(dictHeytingAlgebra1)(v.value1));
            }
        };
    };
};
var genericTuple = {
    to: function (x) {
        return new Tuple(x.value0, x.value1);
    },
    from: function (x) {
        return new Data_Generic_Rep.Product(x.value0, x.value1);
    }
};
var functorTuple = {
    map: function (f) {
        return function (m) {
            return new Tuple(m.value0, f(m.value1));
        };
    }
};
var invariantTuple = {
    imap: Data_Functor_Invariant.imapF(functorTuple)
};
var fst = function (v) {
    return v.value0;
};
var lazyTuple = function (dictLazy) {
    return function (dictLazy1) {
        return {
            defer: function (f) {
                return new Tuple(Control_Lazy.defer(dictLazy)(function (v) {
                    return fst(f(Data_Unit.unit));
                }), Control_Lazy.defer(dictLazy1)(function (v) {
                    return snd(f(Data_Unit.unit));
                }));
            }
        };
    };
};
var extendTuple = {
    extend: function (f) {
        return function (v) {
            return new Tuple(v.value0, f(v));
        };
    },
    Functor0: function () {
        return functorTuple;
    }
};
var eqTuple = function (dictEq) {
    return function (dictEq1) {
        return {
            eq: function (x) {
                return function (y) {
                    return Data_Eq.eq(dictEq)(x.value0)(y.value0) && Data_Eq.eq(dictEq1)(x.value1)(y.value1);
                };
            }
        };
    };
};
var ordTuple = function (dictOrd) {
    return function (dictOrd1) {
        return {
            compare: function (x) {
                return function (y) {
                    var v = Data_Ord.compare(dictOrd)(x.value0)(y.value0);
                    if (v instanceof Data_Ordering.LT) {
                        return Data_Ordering.LT.value;
                    };
                    if (v instanceof Data_Ordering.GT) {
                        return Data_Ordering.GT.value;
                    };
                    return Data_Ord.compare(dictOrd1)(x.value1)(y.value1);
                };
            },
            Eq0: function () {
                return eqTuple(dictOrd.Eq0())(dictOrd1.Eq0());
            }
        };
    };
};
var eq1Tuple = function (dictEq) {
    return {
        eq1: function (dictEq1) {
            return Data_Eq.eq(eqTuple(dictEq)(dictEq1));
        }
    };
};
var ord1Tuple = function (dictOrd) {
    return {
        compare1: function (dictOrd1) {
            return Data_Ord.compare(ordTuple(dictOrd)(dictOrd1));
        },
        Eq10: function () {
            return eq1Tuple(dictOrd.Eq0());
        }
    };
};
var curry = function (f) {
    return function (a) {
        return function (b) {
            return f(new Tuple(a, b));
        };
    };
};
var comonadTuple = {
    extract: snd,
    Extend0: function () {
        return extendTuple;
    }
};
var commutativeRingTuple = function (dictCommutativeRing) {
    return function (dictCommutativeRing1) {
        return {
            Ring0: function () {
                return ringTuple(dictCommutativeRing.Ring0())(dictCommutativeRing1.Ring0());
            }
        };
    };
};
var boundedTuple = function (dictBounded) {
    return function (dictBounded1) {
        return {
            top: new Tuple(Data_Bounded.top(dictBounded), Data_Bounded.top(dictBounded1)),
            bottom: new Tuple(Data_Bounded.bottom(dictBounded), Data_Bounded.bottom(dictBounded1)),
            Ord0: function () {
                return ordTuple(dictBounded.Ord0())(dictBounded1.Ord0());
            }
        };
    };
};
var booleanAlgebraTuple = function (dictBooleanAlgebra) {
    return function (dictBooleanAlgebra1) {
        return {
            HeytingAlgebra0: function () {
                return heytingAlgebraTuple(dictBooleanAlgebra.HeytingAlgebra0())(dictBooleanAlgebra1.HeytingAlgebra0());
            }
        };
    };
};
var applyTuple = function (dictSemigroup) {
    return {
        apply: function (v) {
            return function (v1) {
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v.value1(v1.value1));
            };
        },
        Functor0: function () {
            return functorTuple;
        }
    };
};
var bindTuple = function (dictSemigroup) {
    return {
        bind: function (v) {
            return function (f) {
                var v1 = f(v.value1);
                return new Tuple(Data_Semigroup.append(dictSemigroup)(v.value0)(v1.value0), v1.value1);
            };
        },
        Apply0: function () {
            return applyTuple(dictSemigroup);
        }
    };
};
var applicativeTuple = function (dictMonoid) {
    return {
        pure: Tuple.create(Data_Monoid.mempty(dictMonoid)),
        Apply0: function () {
            return applyTuple(dictMonoid.Semigroup0());
        }
    };
};
var monadTuple = function (dictMonoid) {
    return {
        Applicative0: function () {
            return applicativeTuple(dictMonoid);
        },
        Bind1: function () {
            return bindTuple(dictMonoid.Semigroup0());
        }
    };
};
module.exports = {
    Tuple: Tuple,
    fst: fst,
    snd: snd,
    curry: curry,
    uncurry: uncurry,
    swap: swap,
    showTuple: showTuple,
    eqTuple: eqTuple,
    eq1Tuple: eq1Tuple,
    ordTuple: ordTuple,
    ord1Tuple: ord1Tuple,
    boundedTuple: boundedTuple,
    semigroupoidTuple: semigroupoidTuple,
    semigroupTuple: semigroupTuple,
    monoidTuple: monoidTuple,
    semiringTuple: semiringTuple,
    ringTuple: ringTuple,
    commutativeRingTuple: commutativeRingTuple,
    heytingAlgebraTuple: heytingAlgebraTuple,
    booleanAlgebraTuple: booleanAlgebraTuple,
    functorTuple: functorTuple,
    genericTuple: genericTuple,
    invariantTuple: invariantTuple,
    applyTuple: applyTuple,
    applicativeTuple: applicativeTuple,
    bindTuple: bindTuple,
    monadTuple: monadTuple,
    extendTuple: extendTuple,
    comonadTuple: comonadTuple,
    lazyTuple: lazyTuple
};