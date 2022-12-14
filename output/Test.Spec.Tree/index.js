// Generated by purs version 0.14.5
"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_State = require("../Control.Monad.State/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Array_NonEmpty = require("../Data.Array.NonEmpty/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Node = (function () {
    function Node(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Node.create = function (value0) {
        return function (value1) {
            return new Node(value0, value1);
        };
    };
    return Node;
})();
var Leaf = (function () {
    function Leaf(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Leaf.create = function (value0) {
        return function (value1) {
            return new Leaf(value0, value1);
        };
    };
    return Leaf;
})();
var PathItem = function (x) {
    return x;
};
var Item = function (x) {
    return x;
};
var treeFoldable = {
    foldr: function (f) {
        return function (i) {
            return function (v) {
                if (v instanceof Leaf) {
                    return Data_Maybe.maybe(i)(function (a$prime) {
                        return f(a$prime)(i);
                    })(v.value1);
                };
                if (v instanceof Node) {
                    return Data_Foldable.foldr(Data_Foldable.foldableArray)(function (a) {
                        return function (i$prime) {
                            return Data_Foldable.foldr(treeFoldable)(f)(i$prime)(a);
                        };
                    })(i)(v.value1);
                };
                throw new Error("Failed pattern match at Test.Spec.Tree (line 57, column 1 - line 62, column 32): " + [ f.constructor.name, i.constructor.name, v.constructor.name ]);
            };
        };
    },
    foldl: function (f) {
        return function (i) {
            return function (v) {
                if (v instanceof Leaf) {
                    return Data_Maybe.maybe(i)(function (a$prime) {
                        return f(i)(a$prime);
                    })(v.value1);
                };
                if (v instanceof Node) {
                    return Data_Foldable.foldl(Data_Foldable.foldableArray)(function (i$prime) {
                        return function (a) {
                            return Data_Foldable.foldl(treeFoldable)(f)(i$prime)(a);
                        };
                    })(i)(v.value1);
                };
                throw new Error("Failed pattern match at Test.Spec.Tree (line 57, column 1 - line 62, column 32): " + [ f.constructor.name, i.constructor.name, v.constructor.name ]);
            };
        };
    },
    foldMap: function (dictMonoid) {
        return function (f) {
            return Data_Foldable.foldMapDefaultL(treeFoldable)(dictMonoid)(f);
        };
    }
};
var showIdTerm = Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
    reflectSymbol: function () {
        return "index";
    }
})(Data_Show.showRecordFieldsCons({
    reflectSymbol: function () {
        return "name";
    }
})(Data_Show.showRecordFieldsNil)(Data_Maybe.showMaybe(Data_Show.showString)))(Data_Show.showInt));
var showGroup = function (dictShow) {
    return function (dictShow1) {
        return {
            show: function (v) {
                if (v instanceof Node) {
                    return "(Node " + (Data_Show.show(Data_Either.showEither(Data_Show.showString)(dictShow))(v.value0) + (" " + (Data_Show.show(Data_Show.showArray(showGroup(dictShow)(dictShow1)))(v.value1) + ")")));
                };
                if (v instanceof Leaf) {
                    return "(Leaf " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Maybe.showMaybe(dictShow1))(v.value1) + ")")));
                };
                throw new Error("Failed pattern match at Test.Spec.Tree (line 35, column 1 - line 37, column 69): " + [ v.constructor.name ]);
            }
        };
    };
};
var pathItemOrd = Data_Ord.ordRecord()(Data_Ord.ordRecordCons(Data_Ord.ordRecordCons(Data_Ord.ordRecordNil)()({
    reflectSymbol: function () {
        return "name";
    }
})(Data_Maybe.ordMaybe(Data_Ord.ordString)))()({
    reflectSymbol: function () {
        return "index";
    }
})(Data_Ord.ordInt));
var pathItemEq = Data_Eq.eqRec()(Data_Eq.eqRowCons(Data_Eq.eqRowCons(Data_Eq.eqRowNil)()({
    reflectSymbol: function () {
        return "name";
    }
})(Data_Maybe.eqMaybe(Data_Eq.eqString)))()({
    reflectSymbol: function () {
        return "index";
    }
})(Data_Eq.eqInt));
var parentSuite = Data_Function.flip(Data_Foldable.foldr(Data_Foldable.foldableArray))(Data_Maybe.Nothing.value)(function (v) {
    return function (v1) {
        if (v.name instanceof Data_Maybe.Just && v1 instanceof Data_Maybe.Nothing) {
            return new Data_Maybe.Just({
                path: [  ],
                name: v.name.value0
            });
        };
        if (v.name instanceof Data_Maybe.Nothing && v1 instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        if (v1 instanceof Data_Maybe.Just) {
            return new Data_Maybe.Just({
                path: Data_Semigroup.append(Data_Semigroup.semigroupArray)([ v ])(v1.value0.path),
                name: v1.value0.name
            });
        };
        throw new Error("Failed pattern match at Test.Spec.Tree (line 131, column 34 - line 134, column 50): " + [ v.constructor.name, v1.constructor.name ]);
    };
});
var newtypePathItem = {
    Coercible0: function () {
        return undefined;
    }
};
var parentSuiteName = Data_Array.mapMaybe((function () {
    var $95 = Data_Newtype.un()(PathItem);
    return function ($96) {
        return (function (v) {
            return v.name;
        })($95($96));
    };
})());
var modifyAroundAction = function (action) {
    return function (v) {
        return Item({
            isFocused: v.isFocused,
            isParallelizable: v.isParallelizable,
            example: function (aroundAction) {
                return v.example(function ($97) {
                    return aroundAction(action($97));
                });
            }
        });
    };
};
var itemShow = {
    show: function (v) {
        return "Item (" + (Data_Show.show(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
            reflectSymbol: function () {
                return "example";
            }
        })(Data_Show.showRecordFieldsCons({
            reflectSymbol: function () {
                return "isFocused";
            }
        })(Data_Show.showRecordFieldsCons({
            reflectSymbol: function () {
                return "isParallelizable";
            }
        })(Data_Show.showRecordFieldsNil)(Data_Maybe.showMaybe(Data_Show.showBoolean)))(Data_Show.showBoolean))(Data_Show.showString)))({
            isFocused: v.isFocused,
            isParallelizable: v.isParallelizable,
            example: "Function"
        }) + ")");
    }
};
var itemNewtype = {
    Coercible0: function () {
        return undefined;
    }
};
var itemEq = {
    eq: function (v) {
        return function (v1) {
            return v.isFocused === v1.isFocused && Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqBoolean))(v.isParallelizable)(v1.isParallelizable);
        };
    }
};
var isAllParallelizable = function (v) {
    if (v instanceof Node) {
        return Data_Foldable.all(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(isAllParallelizable)(v.value1);
    };
    if (v instanceof Leaf) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(itemEq))(v.value1)(Data_Maybe.Nothing.value) || Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqBoolean))(Control_Bind.bind(Data_Maybe.bindMaybe)(v.value1)((function () {
            var $98 = Data_Newtype.un()(Item);
            return function ($99) {
                return (function (v1) {
                    return v1.isParallelizable;
                })($98($99));
            };
        })()))(new Data_Maybe.Just(true));
    };
    throw new Error("Failed pattern match at Test.Spec.Tree (line 92, column 23 - line 94, column 82): " + [ v.constructor.name ]);
};
var eqGroup = function (dictEq) {
    return function (dictEq1) {
        return {
            eq: function (v) {
                return function (v1) {
                    if (v instanceof Node && v1 instanceof Node) {
                        return Data_Eq.eq(Data_Either.eqEither(Data_Eq.eqString)(dictEq))(v.value0)(v1.value0) && Data_Eq.eq(Data_Eq.eqArray(eqGroup(dictEq)(dictEq1)))(v.value1)(v1.value1);
                    };
                    if (v instanceof Leaf && v1 instanceof Leaf) {
                        return v.value0 === v1.value0 && Data_Eq.eq(Data_Maybe.eqMaybe(dictEq1))(v.value1)(v1.value1);
                    };
                    return false;
                };
            }
        };
    };
};
var discardUnfocused = function (ts) {
    var findFocus = function (v) {
        if (v instanceof Node) {
            var v1 = Data_Array.mapMaybe(findFocus)(v.value1);
            if (v1.length === 0) {
                return Data_Maybe.Nothing.value;
            };
            return Data_Maybe.Just.create(new Node(v.value0, v1));
        };
        if (v instanceof Leaf && v.value1 instanceof Data_Maybe.Just) {
            if (v.value1.value0.isFocused) {
                return new Data_Maybe.Just(v);
            };
            return Data_Maybe.Nothing.value;
        };
        if (v instanceof Leaf && v.value1 instanceof Data_Maybe.Nothing) {
            return Data_Maybe.Nothing.value;
        };
        throw new Error("Failed pattern match at Test.Spec.Tree (line 105, column 3 - line 105, column 62): " + [ v.constructor.name ]);
    };
    var v = Data_Array.mapMaybe(findFocus)(ts);
    if (v.length === 0) {
        return ts;
    };
    return v;
};
var countTests = function (g) {
    var go = function (v) {
        if (v instanceof Node) {
            return Data_Foldable.for_(Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(Data_Foldable.foldableArray)(v.value1)(go);
        };
        if (v instanceof Leaf) {
            return Control_Monad_State_Class.modify_(Control_Monad_State_Trans.monadStateStateT(Data_Identity.monadIdentity))(function (v1) {
                return v1 + 1 | 0;
            });
        };
        throw new Error("Failed pattern match at Test.Spec.Tree (line 86, column 3 - line 86, column 30): " + [ v.constructor.name ]);
    };
    return Control_Monad_State.execState(Data_Traversable["for"](Control_Monad_State_Trans.applicativeStateT(Data_Identity.monadIdentity))(Data_Traversable.traversableArray)(g)(go))(0);
};
var bimapTree = function (g) {
    return function (f) {
        var go = function (namePath) {
            return function (spec) {
                if (spec instanceof Node) {
                    var namePath$prime = Data_Either.either(Data_Array.snoc(namePath))(Data_Function["const"](namePath))(spec.value0);
                    return new Node(Data_Functor.map(Data_Either.functorEither)(g(namePath$prime))(spec.value0), Data_Functor.map(Data_Functor.functorArray)(go(namePath$prime))(spec.value1));
                };
                if (spec instanceof Leaf) {
                    return new Leaf(spec.value0, Data_Functor.map(Data_Maybe.functorMaybe)(f(Data_Array_NonEmpty["snoc'"](namePath)(spec.value0)))(spec.value1));
                };
                throw new Error("Failed pattern match at Test.Spec.Tree (line 48, column 24 - line 52, column 66): " + [ spec.constructor.name ]);
            };
        };
        return go([  ]);
    };
};
var treeBifunctor = {
    bimap: function (g) {
        return function (f) {
            return bimapTree(Data_Function["const"](g))(Data_Function["const"](f));
        };
    }
};
module.exports = {
    Node: Node,
    Leaf: Leaf,
    Item: Item,
    bimapTree: bimapTree,
    countTests: countTests,
    isAllParallelizable: isAllParallelizable,
    discardUnfocused: discardUnfocused,
    modifyAroundAction: modifyAroundAction,
    PathItem: PathItem,
    parentSuiteName: parentSuiteName,
    parentSuite: parentSuite,
    showGroup: showGroup,
    eqGroup: eqGroup,
    treeBifunctor: treeBifunctor,
    treeFoldable: treeFoldable,
    itemNewtype: itemNewtype,
    itemShow: itemShow,
    itemEq: itemEq,
    newtypePathItem: newtypePathItem,
    showIdTerm: showIdTerm,
    pathItemEq: pathItemEq,
    pathItemOrd: pathItemOrd
};
