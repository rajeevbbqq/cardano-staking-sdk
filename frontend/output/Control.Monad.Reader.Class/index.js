// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var monadAskFun = {
    ask: Control_Category.identity(Control_Category.categoryFn),
    Monad0: function () {
        return Control_Monad.monadFn;
    }
};
var monadReaderFun = {
    local: Control_Semigroupoid.composeFlipped(Control_Semigroupoid.semigroupoidFn),
    MonadAsk0: function () {
        return monadAskFun;
    }
};
var local = function (dict) {
    return dict.local;
};
var ask = function (dict) {
    return dict.ask;
};
var asks = function (dictMonadAsk) {
    return function (f) {
        return Data_Functor.map((((dictMonadAsk.Monad0()).Bind1()).Apply0()).Functor0())(f)(ask(dictMonadAsk));
    };
};
module.exports = {
    ask: ask,
    local: local,
    asks: asks,
    monadAskFun: monadAskFun,
    monadReaderFun: monadReaderFun
};
