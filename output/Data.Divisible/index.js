// Generated by purs version 0.14.5
"use strict";
var Data_Comparison = require("../Data.Comparison/index.js");
var Data_Divide = require("../Data.Divide/index.js");
var Data_Equivalence = require("../Data.Equivalence/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Op = require("../Data.Op/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var divisiblePredicate = {
    conquer: Data_Function["const"](true),
    Divide0: function () {
        return Data_Divide.dividePredicate;
    }
};
var divisibleOp = function (dictMonoid) {
    return {
        conquer: Data_Op.Op(Data_Function["const"](Data_Monoid.mempty(dictMonoid))),
        Divide0: function () {
            return Data_Divide.divideOp(dictMonoid.Semigroup0());
        }
    };
};
var divisibleEquivalence = {
    conquer: Data_Equivalence.Equivalence(function (v) {
        return function (v1) {
            return true;
        };
    }),
    Divide0: function () {
        return Data_Divide.divideEquivalence;
    }
};
var divisibleComparison = {
    conquer: Data_Comparison.Comparison(function (v) {
        return function (v1) {
            return Data_Ordering.EQ.value;
        };
    }),
    Divide0: function () {
        return Data_Divide.divideComparison;
    }
};
var conquer = function (dict) {
    return dict.conquer;
};
module.exports = {
    conquer: conquer,
    divisibleComparison: divisibleComparison,
    divisibleEquivalence: divisibleEquivalence,
    divisiblePredicate: divisiblePredicate,
    divisibleOp: divisibleOp
};
