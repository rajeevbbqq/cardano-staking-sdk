// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Control_Semigroupoid = require("../Control.Semigroupoid/index.js");
var Data_Show = require("../Data.Show/index.js");
var Endo = function (x) {
    return x;
};
var showEndo = function (dictShow) {
    return {
        show: function (v) {
            return "(Endo " + (Data_Show.show(dictShow)(v) + ")");
        }
    };
};
var semigroupEndo = function (dictSemigroupoid) {
    return {
        append: function (v) {
            return function (v1) {
                return Control_Semigroupoid.compose(dictSemigroupoid)(v)(v1);
            };
        }
    };
};
var ordEndo = function (dictOrd) {
    return dictOrd;
};
var monoidEndo = function (dictCategory) {
    return {
        mempty: Control_Category.identity(dictCategory),
        Semigroup0: function () {
            return semigroupEndo(dictCategory.Semigroupoid0());
        }
    };
};
var eqEndo = function (dictEq) {
    return dictEq;
};
var boundedEndo = function (dictBounded) {
    return dictBounded;
};
module.exports = {
    Endo: Endo,
    eqEndo: eqEndo,
    ordEndo: ordEndo,
    boundedEndo: boundedEndo,
    showEndo: showEndo,
    semigroupEndo: semigroupEndo,
    monoidEndo: monoidEndo
};
