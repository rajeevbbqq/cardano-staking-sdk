// Generated by purs version 0.14.5
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var Test_QuickCheck = require("../Test.QuickCheck/index.js");
var checkOrd = function (dictArbitrary) {
    return function (dictOrd) {
        return function (v) {
            var transitivity = function (a) {
                return function (b) {
                    return function (c) {
                        var $3 = Data_Ord.lessThanOrEq(dictOrd)(a)(b) && Data_Ord.lessThanOrEq(dictOrd)(b)(c);
                        if ($3) {
                            return Data_Ord.lessThanOrEq(dictOrd)(a)(c);
                        };
                        return true;
                    };
                };
            };
            var reflexivity = function (a) {
                return Data_Ord.lessThanOrEq(dictOrd)(a)(a);
            };
            var antisymmetry = function (a) {
                return function (b) {
                    var $4 = Data_Ord.lessThanOrEq(dictOrd)(a)(b) && Data_Ord.lessThanOrEq(dictOrd)(b)(a);
                    if ($4) {
                        return Data_Eq.eq(dictOrd.Eq0())(a)(b);
                    };
                    return Data_Eq.notEq(dictOrd.Eq0())(a)(b);
                };
            };
            return function __do() {
                Effect_Console.log("Checking 'Reflexivity' law for Ord")();
                Test_QuickCheck["quickCheck'"](Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableBoolean))(1000)(reflexivity)();
                Effect_Console.log("Checking 'Antisymmetry' law for Ord")();
                Test_QuickCheck["quickCheck'"](Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableBoolean)))(1000)(antisymmetry)();
                Effect_Console.log("Checking 'Transitivity' law for Ord")();
                return Test_QuickCheck["quickCheck'"](Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableFunction(dictArbitrary)(Test_QuickCheck.testableBoolean))))(1000)(transitivity)();
            };
        };
    };
};
module.exports = {
    checkOrd: checkOrd
};
