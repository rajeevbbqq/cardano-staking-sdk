// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_These = require("../Data.These/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var FromData = require("../FromData/index.js");
var ToData = require("../ToData/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var $$Map = function (x) {
    return x;
};
var toDataMap = function (dictToData) {
    return function (dictToData1) {
        return {
            toData: function (v) {
                return new Types_PlutusData["Map"](Data_Functor.map(Data_Functor.functorArray)(Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple)(ToData.toData(dictToData))(ToData.toData(dictToData1)))(v));
            }
        };
    };
};
var ordMap = function (dictOrd) {
    return function (dictOrd1) {
        return Data_Ord.ordArray(Data_Tuple.ordTuple(dictOrd)(dictOrd1));
    };
};
var newtypeMap_ = {
    Coercible0: function () {
        return undefined;
    }
};
var genericMap_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showMap = function (dictShow) {
    return function (dictShow1) {
        return {
            show: Data_Show_Generic.genericShow(genericMap_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showArray(Data_Tuple.showTuple(dictShow)(dictShow1))))({
                reflectSymbol: function () {
                    return "Map";
                }
            }))
        };
    };
};
var functorMap = {
    map: function (f) {
        return function (v) {
            return $$Map(Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Data_Tuple.functorTuple)(f))(v));
        };
    }
};
var functorWithIndexMap = {
    mapWithIndex: function (f) {
        return function (v) {
            return $$Map(Data_Functor.map(Data_Functor.functorArray)(function (v1) {
                return new Data_Tuple.Tuple(v1.value0, f(v1.value0)(v1.value1));
            })(v));
        };
    },
    Functor0: function () {
        return functorMap;
    }
};
var fromDataMap = function (dictFromData) {
    return function (dictFromData1) {
        return {
            fromData: function (v) {
                if (v instanceof Types_PlutusData["Map"]) {
                    return Data_Functor.map(Data_Maybe.functorMaybe)($$Map)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(v.value0)(function (v1) {
                        return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(FromData.fromData(dictFromData)(v1.value0)))(FromData.fromData(dictFromData1)(v1.value1));
                    }));
                };
                return Data_Maybe.Nothing.value;
            }
        };
    };
};
var foldableMap = {
    foldMap: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_Foldable.foldMap(Data_Foldable.foldableArray)(dictMonoid)(Data_Foldable.foldMap(Data_Foldable.foldableTuple)(dictMonoid)(f))(v);
            };
        };
    },
    foldr: function (f) {
        return Data_Foldable.foldrDefault(foldableMap)(f);
    },
    foldl: function (f) {
        return Data_Foldable.foldlDefault(foldableMap)(f);
    }
};
var foldableWithIndexMap = {
    foldMapWithIndex: function (dictMonoid) {
        return function (f) {
            return function (v) {
                return Data_Foldable.foldMap(Data_Foldable.foldableArray)(dictMonoid)(Data_Tuple.uncurry(f))(v);
            };
        };
    },
    foldrWithIndex: function (f) {
        return Data_FoldableWithIndex.foldrWithIndexDefault(foldableWithIndexMap)(f);
    },
    foldlWithIndex: function (f) {
        return Data_FoldableWithIndex.foldlWithIndexDefault(foldableWithIndexMap)(f);
    },
    Foldable0: function () {
        return foldableMap;
    }
};
var traversableMap = {
    traverse: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())($$Map)(Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(Data_Traversable.traverse(Data_Traversable.traversableTuple)(dictApplicative)(f))(v));
            };
        };
    },
    sequence: function (dictApplicative) {
        return function (v) {
            return Data_Functor.map((dictApplicative.Apply0()).Functor0())($$Map)(Data_Traversable.sequence(Data_Traversable.traversableArray)(dictApplicative)(Data_Functor.map(Data_Functor.functorArray)(Data_Traversable.sequence(Data_Traversable.traversableTuple)(dictApplicative))(v)));
        };
    },
    Functor0: function () {
        return functorMap;
    },
    Foldable1: function () {
        return foldableMap;
    }
};
var traversableWithIndexMap = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())($$Map)(Data_Traversable.traverse(Data_Traversable.traversableArray)(dictApplicative)(function (v1) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(v1.value0))(f(v1.value0)(v1.value1));
                })(v));
            };
        };
    },
    FunctorWithIndex0: function () {
        return functorWithIndexMap;
    },
    FoldableWithIndex1: function () {
        return foldableWithIndexMap;
    },
    Traversable2: function () {
        return traversableMap;
    }
};
var eqMap = function (dictEq) {
    return function (dictEq1) {
        return Data_Eq.eqArray(Data_Tuple.eqTuple(dictEq)(dictEq1));
    };
};
var encodeAesonMap = function (dictEncodeAeson) {
    return function (dictEncodeAeson1) {
        return Aeson.encodeAesonArray(Aeson.encodeAesonTuple(dictEncodeAeson)(dictEncodeAeson1));
    };
};
var decodeAesonMap = function (dictDecodeAeson) {
    return function (dictDecodeAeson1) {
        return Aeson.decodeAesonArray(Aeson.decodeAesonTuple(dictDecodeAeson)(dictDecodeAeson1));
    };
};
var values = function (v) {
    return Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.snd)(v);
};
var singleton = function (k) {
    return function (v) {
        return $$Map(Data_Array.singleton(new Data_Tuple.Tuple(k, v)));
    };
};
var $$null = function (v) {
    return Data_Array["null"](v);
};
var mapThese = function (f) {
    return function (mps) {
        var f$prime = function (v) {
            return function (v1) {
                if (v.value1 instanceof Data_These.This) {
                    return new Data_Tuple.Tuple(Data_Array.cons(new Data_Tuple.Tuple(v.value0, v.value1.value0))(v1.value0), v1.value1);
                };
                if (v.value1 instanceof Data_These.That) {
                    return new Data_Tuple.Tuple(v1.value0, Data_Array.cons(new Data_Tuple.Tuple(v.value0, v.value1.value0))(v1.value1));
                };
                if (v.value1 instanceof Data_These.Both) {
                    return new Data_Tuple.Tuple(Data_Array.cons(new Data_Tuple.Tuple(v.value0, v.value1.value0))(v1.value0), Data_Array.cons(new Data_Tuple.Tuple(v.value0, v.value1.value1))(v1.value1));
                };
                throw new Error("Failed pattern match at Plutus.Types.AssocMap (line 193, column 28 - line 196, column 54): " + [ v.value1.constructor.name ]);
            };
        };
        var mappedThese = Data_Foldable.foldr(Data_Foldable.foldableArray)(f$prime)(new Data_Tuple.Tuple([  ], [  ]))(Data_Newtype.unwrap()(Data_Functor.map(functorMap)(f)(mps)));
        return new Data_Tuple.Tuple(Data_Tuple.fst(mappedThese), Data_Tuple.snd(mappedThese));
    };
};
var mapMaybeWithKey = function (f) {
    return function (v) {
        return $$Map(Data_Array.mapMaybe(function (v1) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                return new Data_Tuple.Tuple(v1.value0, v3);
            })(f(v1.value0)(v1.value1));
        })(v));
    };
};
var mapMaybe = function (f) {
    return function (v) {
        return $$Map(Data_Array.mapMaybe(function (v1) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(function (v3) {
                return new Data_Tuple.Tuple(v1.value0, v3);
            })(f(v1.value1));
        })(v));
    };
};
var lookup = function (dictEq) {
    return function (k) {
        return function (v) {
            return Data_Foldable.lookup(Data_Foldable.foldableArray)(dictEq)(k)(v);
        };
    };
};
var member = function (dictEq) {
    return function (k) {
        var $156 = lookup(dictEq)(k);
        return function ($157) {
            return Data_Maybe.isJust($156($157));
        };
    };
};
var union = function (dictEq) {
    return function (v) {
        return function (v1) {
            var rs$prime = Data_Array.filter(function (v2) {
                return !Data_Array.any(function (v3) {
                    return Data_Eq.eq(dictEq)(v3.value0)(v2.value0);
                })(v);
            })(v1);
            var rs$prime$prime = Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Data_Tuple.functorTuple)(Data_These.That.create))(rs$prime);
            var f = function (a) {
                return function (v2) {
                    if (v2 instanceof Data_Maybe.Nothing) {
                        return new Data_These.This(a);
                    };
                    if (v2 instanceof Data_Maybe.Just) {
                        return new Data_These.Both(a, v2.value0);
                    };
                    throw new Error("Failed pattern match at Plutus.Types.AssocMap (line 155, column 11 - line 157, column 25): " + [ v2.constructor.name ]);
                };
            };
            var ls$prime = Data_Functor.map(Data_Functor.functorArray)(function (v2) {
                return new Data_Tuple.Tuple(v2.value0, f(v2.value1)(lookup(dictEq)(v2.value0)(v1)));
            })(v);
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(ls$prime)(rs$prime$prime);
        };
    };
};
var unionWith = function (dictEq) {
    return function (merge) {
        return function (ls) {
            return function (rs) {
                return Data_Functor.map(functorMap)(Data_These.these(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn))(merge))(union(dictEq)(ls)(rs));
            };
        };
    };
};
var semigroupMap = function (dictEq) {
    return function (dictSemigroup) {
        return {
            append: unionWith(dictEq)(Data_Semigroup.append(dictSemigroup))
        };
    };
};
var keys = function (v) {
    return Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.fst)(v);
};
var insert = function (dictEq) {
    return function (k) {
        return function (v) {
            return function (m) {
                return unionWith(dictEq)(function (v1) {
                    return function (b) {
                        return b;
                    };
                })(m)(singleton(k)(v));
            };
        };
    };
};
var filter = function (f) {
    return function (v) {
        return $$Map(Data_Array.filter(function ($158) {
            return f(Data_Tuple.snd($158));
        })(v));
    };
};
var empty = [  ];
var monoidMap = function (dictEq) {
    return function (dictSemigroup) {
        return {
            mempty: empty,
            Semigroup0: function () {
                return semigroupMap(dictEq)(dictSemigroup);
            }
        };
    };
};
var elems = function (v) {
    return Data_Functor.map(Data_Functor.functorArray)(Data_Tuple.snd)(v);
};
var $$delete = function (dictEq) {
    return function (k) {
        return function (v) {
            var v1 = Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Array.findIndex((function () {
                var $159 = Data_Eq.eq(dictEq)(k);
                return function ($160) {
                    return $159(Data_Tuple.fst($160));
                };
            })())(v))(Data_Function.flip(Data_Array.deleteAt)(v));
            if (v1 instanceof Data_Maybe.Nothing) {
                return v;
            };
            if (v1 instanceof Data_Maybe.Just) {
                return v1.value0;
            };
            throw new Error("Failed pattern match at Plutus.Types.AssocMap (line 133, column 3 - line 135, column 30): " + [ v1.constructor.name ]);
        };
    };
};
module.exports = {
    "Map": $$Map,
    "delete": $$delete,
    elems: elems,
    empty: empty,
    filter: filter,
    insert: insert,
    keys: keys,
    lookup: lookup,
    mapMaybe: mapMaybe,
    mapMaybeWithKey: mapMaybeWithKey,
    mapThese: mapThese,
    member: member,
    "null": $$null,
    singleton: singleton,
    union: union,
    unionWith: unionWith,
    values: values,
    genericMap_: genericMap_,
    newtypeMap_: newtypeMap_,
    eqMap: eqMap,
    ordMap: ordMap,
    encodeAesonMap: encodeAesonMap,
    decodeAesonMap: decodeAesonMap,
    showMap: showMap,
    toDataMap: toDataMap,
    fromDataMap: fromDataMap,
    functorMap: functorMap,
    functorWithIndexMap: functorWithIndexMap,
    foldableMap: foldableMap,
    foldableWithIndexMap: foldableWithIndexMap,
    traversableMap: traversableMap,
    traversableWithIndexMap: traversableWithIndexMap,
    semigroupMap: semigroupMap,
    monoidMap: monoidMap
};
