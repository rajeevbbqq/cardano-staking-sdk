// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Comonad = require("../Control.Comonad/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Free = require("../Control.Monad.Free/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Control_Monad_State = require("../Control.Monad.State/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Lazy = require("../Data.Lazy/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Cofree = function (x) {
    return x;
};
var tail = function (v) {
    return Data_Tuple.snd(Data_Lazy.force(v));
};
var mkCofree = function (a) {
    return function (t) {
        return Data_Lazy.defer(function (v) {
            return new Data_Tuple.Tuple(a, t);
        });
    };
};
var lazyCofree = {
    defer: function (k) {
        return Data_Lazy.defer(function (v) {
            var v1 = k(Data_Unit.unit);
            return Data_Lazy.force(v1);
        });
    }
};
var hoistCofree = function (dictFunctor) {
    return function (nat) {
        return function (v) {
            return Data_Functor.map(Data_Lazy.functorLazy)(Data_Functor.map(Data_Tuple.functorTuple)((function () {
                var $70 = Data_Functor.map(dictFunctor)(hoistCofree(dictFunctor)(nat));
                return function ($71) {
                    return nat($70($71));
                };
            })()))(v);
        };
    };
};
var head = function (v) {
    return Data_Tuple.fst(Data_Lazy.force(v));
};
var functorCofree = function (dictFunctor) {
    return {
        map: function (f) {
            var loop = function (v) {
                return Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                    return new Data_Tuple.Tuple(f(v1.value0), Data_Functor.map(dictFunctor)(loop)(v1.value1));
                })(v);
            };
            return loop;
        }
    };
};
var foldableCofree = function (dictFoldable) {
    return {
        foldr: function (f) {
            var go = function (fa) {
                return function (b) {
                    return f(head(fa))(Data_Foldable.foldr(dictFoldable)(go)(b)(tail(fa)));
                };
            };
            return Data_Function.flip(go);
        },
        foldl: function (f) {
            var go = function (b) {
                return function (fa) {
                    return Data_Foldable.foldl(dictFoldable)(go)(f(b)(head(fa)))(tail(fa));
                };
            };
            return go;
        },
        foldMap: function (dictMonoid) {
            return function (f) {
                var go = function (fa) {
                    return Data_Semigroup.append(dictMonoid.Semigroup0())(f(head(fa)))(Data_Foldable.foldMap(dictFoldable)(dictMonoid)(go)(tail(fa)));
                };
                return go;
            };
        }
    };
};
var traversableCofree = function (dictTraversable) {
    return {
        sequence: function (dictApplicative) {
            return Data_Traversable.traverse(traversableCofree(dictTraversable))(dictApplicative)(Control_Category.identity(Control_Category.categoryFn));
        },
        traverse: function (dictApplicative) {
            return function (f) {
                var loop = function (ta) {
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(mkCofree)(f(head(ta))))(Data_Traversable.traverse(dictTraversable)(dictApplicative)(loop)(tail(ta)));
                };
                return loop;
            };
        },
        Functor0: function () {
            return functorCofree(dictTraversable.Functor0());
        },
        Foldable1: function () {
            return foldableCofree(dictTraversable.Foldable1());
        }
    };
};
var extendCofree = function (dictFunctor) {
    return {
        extend: function (f) {
            var loop = function (v) {
                return Data_Functor.map(Data_Lazy.functorLazy)(function (v1) {
                    return new Data_Tuple.Tuple(f(v), Data_Functor.map(dictFunctor)(loop)(v1.value1));
                })(v);
            };
            return loop;
        },
        Functor0: function () {
            return functorCofree(dictFunctor);
        }
    };
};
var eqCofree = function (dictEq1) {
    return function (dictEq) {
        return {
            eq: function (x) {
                return function (y) {
                    return Data_Eq.eq(dictEq)(head(x))(head(y)) && Data_Eq.eq1(dictEq1)(eqCofree(dictEq1)(dictEq))(tail(x))(tail(y));
                };
            }
        };
    };
};
var ordCofree = function (dictOrd1) {
    return function (dictOrd) {
        return {
            compare: function (x) {
                return function (y) {
                    var v = Data_Ord.compare(dictOrd)(head(x))(head(y));
                    if (v instanceof Data_Ordering.EQ) {
                        return Data_Ord.compare1(dictOrd1)(ordCofree(dictOrd1)(dictOrd))(tail(x))(tail(y));
                    };
                    return v;
                };
            },
            Eq0: function () {
                return eqCofree(dictOrd1.Eq10())(dictOrd.Eq0());
            }
        };
    };
};
var eq1Cofree = function (dictEq1) {
    return {
        eq1: function (dictEq) {
            return Data_Eq.eq(eqCofree(dictEq1)(dictEq));
        }
    };
};
var ord1Cofree = function (dictOrd1) {
    return {
        compare1: function (dictOrd) {
            return Data_Ord.compare(ordCofree(dictOrd1)(dictOrd));
        },
        Eq10: function () {
            return eq1Cofree(dictOrd1.Eq10());
        }
    };
};
var deferCofree = function ($72) {
    return Cofree(Data_Lazy.defer($72));
};
var semigroupCofree = function (dictApply) {
    return function (dictSemigroup) {
        return {
            append: function (x) {
                return function (y) {
                    return deferCofree(function (v) {
                        return new Data_Tuple.Tuple(Data_Semigroup.append(dictSemigroup)(head(x))(head(y)), Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Data_Semigroup.append(semigroupCofree(dictApply)(dictSemigroup)))(tail(x)))(tail(y)));
                    });
                };
            }
        };
    };
};
var monoidCofree = function (dictApplicative) {
    return function (dictMonoid) {
        return {
            mempty: deferCofree(function (v) {
                return new Data_Tuple.Tuple(Data_Monoid.mempty(dictMonoid), Control_Applicative.pure(dictApplicative)(Data_Monoid.mempty(monoidCofree(dictApplicative)(dictMonoid))));
            }),
            Semigroup0: function () {
                return semigroupCofree(dictApplicative.Apply0())(dictMonoid.Semigroup0());
            }
        };
    };
};
var comonadCofree = function (dictFunctor) {
    return {
        extract: head,
        Extend0: function () {
            return extendCofree(dictFunctor);
        }
    };
};
var explore = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (pair) {
            return function (m) {
                return function (w) {
                    var step = function (ff) {
                        return Control_Monad_State_Class.state(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (cof) {
                            return pair(Data_Functor.map(dictFunctor)(Data_Tuple.Tuple.create)(ff))(tail(cof));
                        });
                    };
                    var v = Control_Monad_State.runState(Control_Monad_Free.runFreeM(dictFunctor)(Control_Monad_State_Trans.monadRecStateT(Control_Monad_Rec_Class.monadRecIdentity))(step)(m))(w);
                    return v.value0(Control_Comonad.extract(comonadCofree(dictFunctor1))(v.value1));
                };
            };
        };
    };
};
var exploreM = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (dictMonadRec) {
            return function (pair) {
                return function (m) {
                    return function (w) {
                        var step = function (ff) {
                            return function (cof) {
                                return pair(Data_Functor.map(dictFunctor)(Data_Tuple.Tuple.create)(ff))(tail(cof));
                            };
                        };
                        var $$eval = function (v) {
                            return v.value0(Control_Comonad.extract(comonadCofree(dictFunctor1))(v.value1));
                        };
                        return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())($$eval)(Control_Monad_State_Trans.runStateT(Control_Monad_Free.runFreeM(dictFunctor)(Control_Monad_State_Trans.monadRecStateT(dictMonadRec))(step)(m))(w));
                    };
                };
            };
        };
    };
};
var buildCofree = function (dictFunctor) {
    return function (k) {
        return function (s) {
            return Data_Lazy.defer(function (v) {
                return Data_Functor.map(Data_Tuple.functorTuple)(Data_Functor.map(dictFunctor)(buildCofree(dictFunctor)(k)))(k(s));
            });
        };
    };
};
var unfoldCofree = function (dictFunctor) {
    return function (e) {
        return function (n) {
            return buildCofree(dictFunctor)(function (s) {
                return new Data_Tuple.Tuple(e(s), n(s));
            });
        };
    };
};
var monadCofree = function (dictAlternative) {
    return {
        Applicative0: function () {
            return applicativeCofree(dictAlternative);
        },
        Bind1: function () {
            return bindCofree(dictAlternative);
        }
    };
};
var bindCofree = function (dictAlternative) {
    return {
        bind: function (fa) {
            return function (f) {
                var loop = function (fa$prime) {
                    var fh = f(head(fa$prime));
                    return mkCofree(head(fh))(Control_Alt.alt((dictAlternative.Plus1()).Alt0())(tail(fh))(Data_Functor.map(((dictAlternative.Plus1()).Alt0()).Functor0())(loop)(tail(fa$prime))));
                };
                return loop(fa);
            };
        },
        Apply0: function () {
            return applyCofree(dictAlternative);
        }
    };
};
var applyCofree = function (dictAlternative) {
    return {
        apply: Control_Monad.ap(monadCofree(dictAlternative)),
        Functor0: function () {
            return functorCofree(((dictAlternative.Plus1()).Alt0()).Functor0());
        }
    };
};
var applicativeCofree = function (dictAlternative) {
    return {
        pure: function (a) {
            return mkCofree(a)(Control_Plus.empty(dictAlternative.Plus1()));
        },
        Apply0: function () {
            return applyCofree(dictAlternative);
        }
    };
};
module.exports = {
    deferCofree: deferCofree,
    mkCofree: mkCofree,
    head: head,
    tail: tail,
    hoistCofree: hoistCofree,
    unfoldCofree: unfoldCofree,
    buildCofree: buildCofree,
    explore: explore,
    exploreM: exploreM,
    semigroupCofree: semigroupCofree,
    monoidCofree: monoidCofree,
    eqCofree: eqCofree,
    eq1Cofree: eq1Cofree,
    ordCofree: ordCofree,
    ord1Cofree: ord1Cofree,
    functorCofree: functorCofree,
    foldableCofree: foldableCofree,
    traversableCofree: traversableCofree,
    extendCofree: extendCofree,
    comonadCofree: comonadCofree,
    applyCofree: applyCofree,
    applicativeCofree: applicativeCofree,
    bindCofree: bindCofree,
    monadCofree: monadCofree,
    lazyCofree: lazyCofree
};