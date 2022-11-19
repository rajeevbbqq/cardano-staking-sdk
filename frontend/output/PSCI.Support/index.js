// Generated by purs version 0.14.5
"use strict";
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Effect = require("../Effect/index.js");
var Effect_Console = require("../Effect.Console/index.js");
var evalShow = function (dictShow) {
    return {
        "eval": Effect_Console.logShow(dictShow)
    };
};
var evalEffectUnit = {
    "eval": Control_Category.identity(Control_Category.categoryFn)
};
var $$eval = function (dict) {
    return dict["eval"];
};
var evalEffect = function (dictEval) {
    return {
        "eval": function (x) {
            return Control_Bind.bind(Effect.bindEffect)(x)($$eval(dictEval));
        }
    };
};
module.exports = {
    "eval": $$eval,
    evalEffectUnit: evalEffectUnit,
    evalEffect: evalEffect,
    evalShow: evalShow
};
