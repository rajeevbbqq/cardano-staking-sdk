// Generated by purs version 0.14.5
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Test_QuickCheck = require("../Test.QuickCheck/index.js");
var checkApply = function (dictApply) {
    return function (dictArbitrary) {
        return function (dictArbitrary1) {
            return function (dictArbitrary2) {
                return function (dictEq) {
                    return function (v) {
                        var associativeComposition = function (f) {
                            return function (g) {
                                return function (x) {
                                    return Data_Eq.eq(dictEq)(Control_Apply.apply(dictApply)(Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(Control_Semigroupoid.compose(Control_Semigroupoid.semigroupoidFn))(f))(g))(x))(Control_Apply.apply(dictApply)(f)(Control_Apply.apply(dictApply)(g)(x)));
                                };
                            };
                        };
                        return function __do() {
                            Effect_Console.log("Checking 'Associative composition' law for Apply")();
                            return Test_QuickCheck["quickCheck'"](Test_QuickCheck.testableFunction(dictArbitrary2)(Test_QuickCheck.testableFunction(dictArbitrary1)(Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableBoolean))))(1000)(associativeComposition)();
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    checkApply: checkApply
};
