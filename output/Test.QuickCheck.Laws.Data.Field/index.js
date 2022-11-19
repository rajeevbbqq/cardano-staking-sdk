// Generated by purs version 0.14.5
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Test_QuickCheck = require("../Test.QuickCheck/index.js");
var checkField = function (dictField) {
    return function (dictArbitrary) {
        return function (dictEq) {
            return function (v) {
                var multiplicativeInverse = function (x) {
                    return function (y) {
                        return Data_Eq.eq(dictEq)(Data_EuclideanRing.mod(dictField.EuclideanRing0())(x)(y))(Data_Semiring.zero(((dictField.DivisionRing1()).Ring0()).Semiring0()));
                    };
                };
                return function __do() {
                    Effect_Console.log("Checking 'Non-zero multiplicative inverse' law for Field")();
                    return Test_QuickCheck["quickCheck'"](Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableBoolean)))(1000)(multiplicativeInverse)();
                };
            };
        };
    };
};
module.exports = {
    checkField: checkField
};