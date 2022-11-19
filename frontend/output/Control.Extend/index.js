// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var extendFn = function (dictSemigroup) {
    return {
        extend: function (f) {
            return function (g) {
                return function (w) {
                    return f(function (w$prime) {
                        return g(Data_Semigroup.append(dictSemigroup)(w)(w$prime));
                    });
                };
            };
        },
        Functor0: function () {
            return Data_Functor.functorFn;
        }
    };
};
var extendArray = {
    extend: $foreign.arrayExtend,
    Functor0: function () {
        return Data_Functor.functorArray;
    }
};
var extend = function (dict) {
    return dict.extend;
};
var extendFlipped = function (dictExtend) {
    return function (w) {
        return function (f) {
            return extend(dictExtend)(f)(w);
        };
    };
};
var duplicate = function (dictExtend) {
    return extend(dictExtend)(Control_Category.identity(Control_Category.categoryFn));
};
var composeCoKleisliFlipped = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return f(extend(dictExtend)(g)(w));
            };
        };
    };
};
var composeCoKleisli = function (dictExtend) {
    return function (f) {
        return function (g) {
            return function (w) {
                return g(extend(dictExtend)(f)(w));
            };
        };
    };
};
module.exports = {
    extend: extend,
    extendFlipped: extendFlipped,
    composeCoKleisli: composeCoKleisli,
    composeCoKleisliFlipped: composeCoKleisliFlipped,
    duplicate: duplicate,
    extendFn: extendFn,
    extendArray: extendArray,
    map: Data_Functor.map,
    "void": Data_Functor["void"]
};
