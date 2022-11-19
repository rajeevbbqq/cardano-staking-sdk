// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Distributive = require("../Data.Distributive/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lens_Types = require("../Data.Lens.Types/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Profunctor = require("../Data.Profunctor/index.js");
var Data_Profunctor_Closed = require("../Data.Profunctor.Closed/index.js");
var zipWithOf = function (g) {
    return function (f) {
        return Data_Newtype.unwrap()(g(f));
    };
};
var zipFWithOf = function (g) {
    return function (f) {
        return Data_Newtype.unwrap()(g(f));
    };
};
var withGrate = function (g) {
    return Data_Newtype.unwrap()(g(function (f) {
        return f(Control_Category.identity(Control_Category.categoryFn));
    }));
};
var grate = function (f) {
    return function (dictClosed) {
        return function (pab) {
            return Data_Profunctor.dimap(dictClosed.Profunctor0())(Data_Function.applyFlipped)(f)(Data_Profunctor_Closed.closed(dictClosed)(pab));
        };
    };
};
var cotraversed = function (dictDistributive) {
    return function (dictClosed) {
        return grate(function (f) {
            return Data_Distributive.cotraverse(dictDistributive)(Data_Functor.functorFn)(f)(Control_Category.identity(Control_Category.categoryFn));
        })(dictClosed);
    };
};
var collectOf = function (dictFunctor) {
    return function (g) {
        return function (f) {
            var $5 = zipFWithOf(g)(Control_Category.identity(Control_Category.categoryFn));
            var $6 = Data_Functor.map(dictFunctor)(f);
            return function ($7) {
                return $5($6($7));
            };
        };
    };
};
var cloneGrate = function (g) {
    return function (dictClosed) {
        return grate(withGrate(g))(dictClosed);
    };
};
module.exports = {
    grate: grate,
    withGrate: withGrate,
    cloneGrate: cloneGrate,
    cotraversed: cotraversed,
    zipWithOf: zipWithOf,
    zipFWithOf: zipFWithOf,
    collectOf: collectOf
};
