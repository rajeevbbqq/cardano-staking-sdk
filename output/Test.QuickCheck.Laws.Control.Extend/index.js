// Generated by purs version 0.14.5
"use strict";
var Control_Extend = require("../Control.Extend/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Test_QuickCheck = require("../Test.QuickCheck/index.js");
var Test_QuickCheck_Arbitrary = require("../Test.QuickCheck.Arbitrary/index.js");
var Test_QuickCheck_Laws = require("../Test.QuickCheck.Laws/index.js");
var checkExtend = function (dictExtend) {
    return function (dictArbitrary) {
        return function (dictCoarbitrary) {
            return function (dictCoarbitrary1) {
                return function (dictEq) {
                    return function (v) {
                        var associativity = function (f) {
                            return function (g) {
                                return function (x) {
                                    return Data_Eq.eq(dictEq)((function (v1) {
                                        return Control_Extend.extend(dictExtend)(f)(v1);
                                    })((function (v1) {
                                        return Control_Extend.extend(dictExtend)(g)(v1);
                                    })(x)))(Control_Extend.extend(dictExtend)(function ($9) {
                                        return f((function (v1) {
                                            return Control_Extend.extend(dictExtend)(g)(v1);
                                        })($9));
                                    })(x));
                                };
                            };
                        };
                        return function __do() {
                            Effect_Console.log("Checking 'Associativity' law for Extend")();
                            return Test_QuickCheck["quickCheck'"](Test_QuickCheck.testableFunction(Test_QuickCheck_Arbitrary.arbFunction(dictCoarbitrary1)(Test_QuickCheck_Laws.arbitraryC))(Test_QuickCheck.testableFunction(Test_QuickCheck_Arbitrary.arbFunction(dictCoarbitrary)(Test_QuickCheck_Laws.arbitraryB))(Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableBoolean))))(1000)(associativity)();
                        };
                    };
                };
            };
        };
    };
};
module.exports = {
    checkExtend: checkExtend
};