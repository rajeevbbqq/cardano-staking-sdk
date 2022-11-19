// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Effect = require("../Effect/index.js");
var key = function (i) {
    var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $1 = $foreign["_key"](i);
    return function ($2) {
        return $0($1($2));
    };
};
var getItem = function (s) {
    var $3 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    var $4 = $foreign["_getItem"](s);
    return function ($5) {
        return $3($4($5));
    };
};
module.exports = {
    key: key,
    getItem: getItem,
    length: $foreign.length,
    setItem: $foreign.setItem,
    removeItem: $foreign.removeItem,
    clear: $foreign.clear
};
