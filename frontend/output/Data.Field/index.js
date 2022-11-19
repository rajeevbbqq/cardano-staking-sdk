// Generated by purs version 0.14.5
"use strict";
var Data_CommutativeRing = require("../Data.CommutativeRing/index.js");
var Data_DivisionRing = require("../Data.DivisionRing/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var field = function (dictEuclideanRing) {
    return function (dictDivisionRing) {
        return {
            EuclideanRing0: function () {
                return dictEuclideanRing;
            },
            DivisionRing1: function () {
                return dictDivisionRing;
            }
        };
    };
};
module.exports = {
    field: field,
    recip: Data_DivisionRing.recip,
    degree: Data_EuclideanRing.degree,
    div: Data_EuclideanRing.div,
    gcd: Data_EuclideanRing.gcd,
    lcm: Data_EuclideanRing.lcm,
    mod: Data_EuclideanRing.mod,
    negate: Data_Ring.negate,
    sub: Data_Ring.sub,
    add: Data_Semiring.add,
    mul: Data_Semiring.mul,
    one: Data_Semiring.one,
    zero: Data_Semiring.zero
};
