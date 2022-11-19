// Generated by purs version 0.14.5
"use strict";
var Data_Unit = require("../Data.Unit/index.js");
var lazyUnit = {
    defer: function (v) {
        return Data_Unit.unit;
    }
};
var lazyFn = {
    defer: function (f) {
        return function (x) {
            return f(Data_Unit.unit)(x);
        };
    }
};
var defer = function (dict) {
    return dict.defer;
};
var fix = function (dictLazy) {
    return function (f) {
        var go = defer(dictLazy)(function (v) {
            return f(go);
        });
        return go;
    };
};
module.exports = {
    defer: defer,
    fix: fix,
    lazyFn: lazyFn,
    lazyUnit: lazyUnit
};
