// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Data_CatQueue = require("../Data.CatQueue/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var CatNil = (function () {
    function CatNil() {

    };
    CatNil.value = new CatNil();
    return CatNil;
})();
var CatCons = (function () {
    function CatCons(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    CatCons.create = function (value0) {
        return function (value1) {
            return new CatCons(value0, value1);
        };
    };
    return CatCons;
})();
var showCatList = function (dictShow) {
    return {
        show: function (v) {
            if (v instanceof CatNil) {
                return "CatNil";
            };
            if (v instanceof CatCons) {
                return "(CatList " + (Data_Show.show(dictShow)(v.value0) + (" " + (Data_Show.show(Data_CatQueue.showCatQueue(showCatList(dictShow)))(v.value1) + ")")));
            };
            throw new Error("Failed pattern match at Data.CatList (line 148, column 1 - line 150, column 71): " + [ v.constructor.name ]);
        }
    };
};
var $$null = function (v) {
    if (v instanceof CatNil) {
        return true;
    };
    return false;
};
var link = function (v) {
    return function (v1) {
        if (v instanceof CatNil) {
            return v1;
        };
        if (v1 instanceof CatNil) {
            return v;
        };
        if (v instanceof CatCons) {
            return new CatCons(v.value0, Data_CatQueue.snoc(v.value1)(v1));
        };
        throw new Error("Failed pattern match at Data.CatList (line 109, column 1 - line 109, column 54): " + [ v.constructor.name, v1.constructor.name ]);
    };
};
var foldr = function (k) {
    return function (b) {
        return function (q) {
            var foldl = function ($copy_v) {
                return function ($copy_c) {
                    return function ($copy_v1) {
                        var $tco_var_v = $copy_v;
                        var $tco_var_c = $copy_c;
                        var $tco_done = false;
                        var $tco_result;
                        function $tco_loop(v, c, v1) {
                            if (v1 instanceof Data_List_Types.Nil) {
                                $tco_done = true;
                                return c;
                            };
                            if (v1 instanceof Data_List_Types.Cons) {
                                $tco_var_v = v;
                                $tco_var_c = v(c)(v1.value0);
                                $copy_v1 = v1.value1;
                                return;
                            };
                            throw new Error("Failed pattern match at Data.CatList (line 125, column 3 - line 125, column 59): " + [ v.constructor.name, c.constructor.name, v1.constructor.name ]);
                        };
                        while (!$tco_done) {
                            $tco_result = $tco_loop($tco_var_v, $tco_var_c, $copy_v1);
                        };
                        return $tco_result;
                    };
                };
            };
            var go = function ($copy_xs) {
                return function ($copy_ys) {
                    var $tco_var_xs = $copy_xs;
                    var $tco_done1 = false;
                    var $tco_result;
                    function $tco_loop(xs, ys) {
                        var v = Data_CatQueue.uncons(xs);
                        if (v instanceof Data_Maybe.Nothing) {
                            $tco_done1 = true;
                            return foldl(function (x) {
                                return function (i) {
                                    return i(x);
                                };
                            })(b)(ys);
                        };
                        if (v instanceof Data_Maybe.Just) {
                            $tco_var_xs = v.value0.value1;
                            $copy_ys = new Data_List_Types.Cons(k(v.value0.value0), ys);
                            return;
                        };
                        throw new Error("Failed pattern match at Data.CatList (line 121, column 14 - line 123, column 67): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done1) {
                        $tco_result = $tco_loop($tco_var_xs, $copy_ys);
                    };
                    return $tco_result;
                };
            };
            return go(q)(Data_List_Types.Nil.value);
        };
    };
};
var uncons = function (v) {
    if (v instanceof CatNil) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof CatCons) {
        return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, (function () {
            var $45 = Data_CatQueue["null"](v.value1);
            if ($45) {
                return CatNil.value;
            };
            return foldr(link)(CatNil.value)(v.value1);
        })()));
    };
    throw new Error("Failed pattern match at Data.CatList (line 100, column 1 - line 100, column 61): " + [ v.constructor.name ]);
};
var foldableCatList = {
    foldMap: function (dictMonoid) {
        return Data_Foldable.foldMapDefaultL(foldableCatList)(dictMonoid);
    },
    foldr: function (f) {
        return function (s) {
            return function (l) {
                return Data_Foldable.foldrDefault(foldableCatList)(f)(s)(l);
            };
        };
    },
    foldl: function (f) {
        var go = function ($copy_acc) {
            return function ($copy_q) {
                var $tco_var_acc = $copy_acc;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(acc, q) {
                    var v = uncons(q);
                    if (v instanceof Data_Maybe.Just) {
                        $tco_var_acc = f(acc)(v.value0.value0);
                        $copy_q = v.value0.value1;
                        return;
                    };
                    if (v instanceof Data_Maybe.Nothing) {
                        $tco_done = true;
                        return acc;
                    };
                    throw new Error("Failed pattern match at Data.CatList (line 157, column 16 - line 159, column 22): " + [ v.constructor.name ]);
                };
                while (!$tco_done) {
                    $tco_result = $tco_loop($tco_var_acc, $copy_q);
                };
                return $tco_result;
            };
        };
        return go;
    }
};
var length = Data_Foldable.length(foldableCatList)(Data_Semiring.semiringInt);
var foldMap = function (dictMonoid) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof CatNil) {
                return Data_Monoid.mempty(dictMonoid);
            };
            if (v1 instanceof CatCons) {
                var d = (function () {
                    var $54 = Data_CatQueue["null"](v1.value1);
                    if ($54) {
                        return CatNil.value;
                    };
                    return foldr(link)(CatNil.value)(v1.value1);
                })();
                return Data_Semigroup.append(dictMonoid.Semigroup0())(v(v1.value0))(foldMap(dictMonoid)(v)(d));
            };
            throw new Error("Failed pattern match at Data.CatList (line 135, column 1 - line 135, column 62): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var empty = CatNil.value;
var append = link;
var cons = function (a) {
    return function (cat) {
        return append(new CatCons(a, Data_CatQueue.empty))(cat);
    };
};
var functorCatList = {
    map: function (v) {
        return function (v1) {
            if (v1 instanceof CatNil) {
                return CatNil.value;
            };
            if (v1 instanceof CatCons) {
                var d = (function () {
                    var $59 = Data_CatQueue["null"](v1.value1);
                    if ($59) {
                        return CatNil.value;
                    };
                    return foldr(link)(CatNil.value)(v1.value1);
                })();
                return cons(v(v1.value0))(Data_Functor.map(functorCatList)(v)(d));
            };
            throw new Error("Failed pattern match at Data.CatList (line 185, column 1 - line 189, column 26): " + [ v.constructor.name, v1.constructor.name ]);
        };
    }
};
var singleton = function (a) {
    return cons(a)(CatNil.value);
};
var traversableCatList = {
    traverse: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof CatNil) {
                    return Control_Applicative.pure(dictApplicative)(CatNil.value);
                };
                if (v1 instanceof CatCons) {
                    var d = (function () {
                        var $64 = Data_CatQueue["null"](v1.value1);
                        if ($64) {
                            return CatNil.value;
                        };
                        return foldr(link)(CatNil.value)(v1.value1);
                    })();
                    return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(cons)(v(v1.value0)))(Data_Traversable.traverse(traversableCatList)(dictApplicative)(v)(d));
                };
                throw new Error("Failed pattern match at Data.CatList (line 175, column 1 - line 183, column 33): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            if (v instanceof CatNil) {
                return Control_Applicative.pure(dictApplicative)(CatNil.value);
            };
            if (v instanceof CatCons) {
                var d = (function () {
                    var $68 = Data_CatQueue["null"](v.value1);
                    if ($68) {
                        return CatNil.value;
                    };
                    return foldr(link)(CatNil.value)(v.value1);
                })();
                return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(cons)(v.value0))(Data_Traversable.sequence(traversableCatList)(dictApplicative)(d));
            };
            throw new Error("Failed pattern match at Data.CatList (line 175, column 1 - line 183, column 33): " + [ v.constructor.name ]);
        };
    },
    Functor0: function () {
        return functorCatList;
    },
    Foldable1: function () {
        return foldableCatList;
    }
};
var semigroupCatList = {
    append: append
};
var monoidCatList = {
    mempty: CatNil.value,
    Semigroup0: function () {
        return semigroupCatList;
    }
};
var monadCatList = {
    Applicative0: function () {
        return applicativeCatList;
    },
    Bind1: function () {
        return bindCatList;
    }
};
var bindCatList = {
    bind: Data_Function.flip(foldMap(monoidCatList)),
    Apply0: function () {
        return applyCatList;
    }
};
var applyCatList = {
    apply: Control_Monad.ap(monadCatList),
    Functor0: function () {
        return functorCatList;
    }
};
var applicativeCatList = {
    pure: singleton,
    Apply0: function () {
        return applyCatList;
    }
};
var fromFoldable = function (dictFoldable) {
    return function (f) {
        return Data_Foldable.foldMap(dictFoldable)(monoidCatList)(singleton)(f);
    };
};
var snoc = function (cat) {
    return function (a) {
        return append(cat)(new CatCons(a, Data_CatQueue.empty));
    };
};
var unfoldable1CatList = {
    unfoldr1: function (f) {
        return function (b) {
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
                        throw new Error("Failed pattern match at Data.CatList (line 171, column 24 - line 173, column 57): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_source, $copy_memo);
                    };
                    return $tco_result;
                };
            };
            return go(b)(CatNil.value);
        };
    }
};
var unfoldableCatList = {
    unfoldr: function (f) {
        return function (b) {
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
                        throw new Error("Failed pattern match at Data.CatList (line 164, column 24 - line 166, column 57): " + [ v.constructor.name ]);
                    };
                    while (!$tco_done) {
                        $tco_result = $tco_loop($tco_var_source, $copy_memo);
                    };
                    return $tco_result;
                };
            };
            return go(b)(CatNil.value);
        };
    },
    Unfoldable10: function () {
        return unfoldable1CatList;
    }
};
var altCatList = {
    alt: append,
    Functor0: function () {
        return functorCatList;
    }
};
var plusCatList = {
    empty: empty,
    Alt0: function () {
        return altCatList;
    }
};
var alternativeCatList = {
    Applicative0: function () {
        return applicativeCatList;
    },
    Plus1: function () {
        return plusCatList;
    }
};
var monadPlusCatList = {
    Monad0: function () {
        return monadCatList;
    },
    Alternative1: function () {
        return alternativeCatList;
    }
};
var monadZeroCatList = {
    Monad0: function () {
        return monadCatList;
    },
    Alternative1: function () {
        return alternativeCatList;
    },
    MonadZeroIsDeprecated2: function () {
        return undefined;
    }
};
module.exports = {
    CatNil: CatNil,
    CatCons: CatCons,
    empty: empty,
    "null": $$null,
    singleton: singleton,
    length: length,
    append: append,
    cons: cons,
    snoc: snoc,
    uncons: uncons,
    fromFoldable: fromFoldable,
    semigroupCatList: semigroupCatList,
    monoidCatList: monoidCatList,
    showCatList: showCatList,
    foldableCatList: foldableCatList,
    unfoldableCatList: unfoldableCatList,
    unfoldable1CatList: unfoldable1CatList,
    traversableCatList: traversableCatList,
    functorCatList: functorCatList,
    applyCatList: applyCatList,
    applicativeCatList: applicativeCatList,
    bindCatList: bindCatList,
    monadCatList: monadCatList,
    altCatList: altCatList,
    plusCatList: plusCatList,
    alternativeCatList: alternativeCatList,
    monadZeroCatList: monadZeroCatList,
    monadPlusCatList: monadPlusCatList
};
