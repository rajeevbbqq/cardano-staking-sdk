// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var poke = function (dictPartial) {
    return $foreign.pokeImpl;
};
var peek = function (dictPartial) {
    return $foreign.peekImpl;
};
module.exports = {
    peek: peek,
    poke: poke
};
