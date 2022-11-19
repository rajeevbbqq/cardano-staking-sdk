// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var whenM = function (dictMonad) {
    return function (mb) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(mb)(function (b) {
                return Control_Applicative.when(dictMonad.Applicative0())(b)(m);
            });
        };
    };
};
var unlessM = function (dictMonad) {
    return function (mb) {
        return function (m) {
            return Control_Bind.bind(dictMonad.Bind1())(mb)(function (b) {
                return Control_Applicative.unless(dictMonad.Applicative0())(b)(m);
            });
        };
    };
};
var monadProxy = {
    Applicative0: function () {
        return Control_Applicative.applicativeProxy;
    },
    Bind1: function () {
        return Control_Bind.bindProxy;
    }
};
var monadFn = {
    Applicative0: function () {
        return Control_Applicative.applicativeFn;
    },
    Bind1: function () {
        return Control_Bind.bindFn;
    }
};
var monadArray = {
    Applicative0: function () {
        return Control_Applicative.applicativeArray;
    },
    Bind1: function () {
        return Control_Bind.bindArray;
    }
};
var liftM1 = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
                return Control_Applicative.pure(dictMonad.Applicative0())(f(a$prime));
            });
        };
    };
};
var ap = function (dictMonad) {
    return function (f) {
        return function (a) {
            return Control_Bind.bind(dictMonad.Bind1())(f)(function (f$prime) {
                return Control_Bind.bind(dictMonad.Bind1())(a)(function (a$prime) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(f$prime(a$prime));
                });
            });
        };
    };
};
module.exports = {
    liftM1: liftM1,
    whenM: whenM,
    unlessM: unlessM,
    ap: ap,
    monadFn: monadFn,
    monadArray: monadArray,
    monadProxy: monadProxy,
    liftA1: Control_Applicative.liftA1,
    pure: Control_Applicative.pure,
    unless: Control_Applicative.unless,
    when: Control_Applicative.when,
    apply: Control_Apply.apply,
    bind: Control_Bind.bind,
    ifM: Control_Bind.ifM,
    join: Control_Bind.join,
    map: Data_Functor.map,
    "void": Data_Functor["void"]
};