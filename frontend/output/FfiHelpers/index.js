// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var untuple = function (v) {
    return [ v.value0, v.value1 ];
};
var maybeFfiHelper = {
    nothing: Data_Maybe.Nothing.value,
    just: Data_Maybe.Just.create,
    from: Data_Maybe.fromMaybe
};
var errorHelper = function (v) {
    return {
        error: function ($4) {
            return Data_Either.Left.create(v($4));
        },
        valid: Data_Either.Right.create,
        from: function (e) {
            var $5 = Data_Maybe.fromMaybe(e);
            return function ($6) {
                return $5(Data_Either.hush($6));
            };
        }
    };
};
var containerHelper = $foreign["_containerHelper"]({
    untuple: untuple,
    tuple: Data_Tuple.Tuple.create
});
module.exports = {
    maybeFfiHelper: maybeFfiHelper,
    containerHelper: containerHelper,
    errorHelper: errorHelper
};
