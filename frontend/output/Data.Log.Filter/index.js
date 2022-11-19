// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Log_Level = require("../Data.Log.Level/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var filterLevel = function (dictMonadEffect) {
    return function (op) {
        return function (level) {
            return function (logger) {
                return function (message) {
                    var $4 = op(message.level)(level);
                    if ($4) {
                        return logger(message);
                    };
                    return Control_Applicative.pure((dictMonadEffect.Monad0()).Applicative0())(Data_Unit.unit);
                };
            };
        };
    };
};
var maximumLevel = function (dictMonadEffect) {
    return filterLevel(dictMonadEffect)(Data_Ord.lessThanOrEq(Data_Log_Level.ordLogLevel));
};
var minimumLevel = function (dictMonadEffect) {
    return filterLevel(dictMonadEffect)(Data_Ord.greaterThanOrEq(Data_Log_Level.ordLogLevel));
};
var onlyLevel = function (dictMonadEffect) {
    return filterLevel(dictMonadEffect)(Data_Eq.eq(Data_Log_Level.eqLogLevel));
};
module.exports = {
    minimumLevel: minimumLevel,
    maximumLevel: maximumLevel,
    onlyLevel: onlyLevel
};