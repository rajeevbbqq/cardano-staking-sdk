// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var prj = function (dict) {
    return dict.prj;
};
var injectReflexive = {
    inj: Control_Category.identity(Control_Category.categoryFn),
    prj: Data_Maybe.Just.create
};
var injectLeft = {
    inj: function ($3) {
        return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($3));
    },
    prj: Data_Functor_Coproduct.coproduct(Data_Maybe.Just.create)(Data_Function["const"](Data_Maybe.Nothing.value))
};
var inj = function (dict) {
    return dict.inj;
};
var injectRight = function (dictInject) {
    return {
        inj: (function () {
            var $4 = inj(dictInject);
            return function ($5) {
                return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create($4($5)));
            };
        })(),
        prj: Data_Functor_Coproduct.coproduct(Data_Function["const"](Data_Maybe.Nothing.value))(prj(dictInject))
    };
};
module.exports = {
    inj: inj,
    prj: prj,
    injectReflexive: injectReflexive,
    injectLeft: injectLeft,
    injectRight: injectRight
};
