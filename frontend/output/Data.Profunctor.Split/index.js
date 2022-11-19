// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Exists = require("../Data.Exists/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor_Invariant = require("../Data.Functor.Invariant/index.js");
var SplitF = (function () {
    function SplitF(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    SplitF.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new SplitF(value0, value1, value2);
            };
        };
    };
    return SplitF;
})();
var Split = function (x) {
    return x;
};
var unSplit = function (f) {
    return function (v) {
        return Data_Exists.runExists(function (v1) {
            return f(v1.value0)(v1.value1)(v1.value2);
        })(v);
    };
};
var split = function (f) {
    return function (g) {
        return function (fx) {
            return Data_Exists.mkExists(new SplitF(f, g, fx));
        };
    };
};
var profunctorSplit = {
    dimap: function (f) {
        return function (g) {
            return unSplit(function (h) {
                return function (i) {
                    return split(function ($9) {
                        return h(f($9));
                    })(function ($10) {
                        return g(i($10));
                    });
                };
            });
        };
    }
};
var lowerSplit = function (dictInvariant) {
    return unSplit(Data_Function.flip(Data_Functor_Invariant.imap(dictInvariant)));
};
var liftSplit = split(Control_Category.identity(Control_Category.categoryFn))(Control_Category.identity(Control_Category.categoryFn));
var hoistSplit = function (nat) {
    return unSplit(function (f) {
        return function (g) {
            var $11 = split(f)(g);
            return function ($12) {
                return $11(nat($12));
            };
        };
    });
};
var functorSplit = {
    map: function (f) {
        return unSplit(function (g) {
            return function (h) {
                return function (fx) {
                    return split(g)(function ($13) {
                        return f(h($13));
                    })(fx);
                };
            };
        });
    }
};
module.exports = {
    split: split,
    unSplit: unSplit,
    liftSplit: liftSplit,
    lowerSplit: lowerSplit,
    hoistSplit: hoistSplit,
    functorSplit: functorSplit,
    profunctorSplit: profunctorSplit
};
