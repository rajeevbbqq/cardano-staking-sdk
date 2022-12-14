// Generated by purs version 0.14.5
"use strict";
var Data_Argonaut_Decode_Class = require("../Data.Argonaut.Decode.Class/index.js");
var Data_Argonaut_Decode_Decoders = require("../Data.Argonaut.Decode.Decoders/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var getFieldOptional$prime = function (dictDecodeJson) {
    return Data_Argonaut_Decode_Decoders["getFieldOptional'"](Data_Argonaut_Decode_Class.decodeJson(dictDecodeJson));
};
var getFieldOptional = function (dictDecodeJson) {
    return Data_Argonaut_Decode_Decoders.getFieldOptional(Data_Argonaut_Decode_Class.decodeJson(dictDecodeJson));
};
var getField = function (dictDecodeJson) {
    return Data_Argonaut_Decode_Decoders.getField(Data_Argonaut_Decode_Class.decodeJson(dictDecodeJson));
};
var defaultField = function (parser) {
    return function ($$default) {
        return Data_Functor.map(Data_Either.functorEither)(Data_Maybe.fromMaybe($$default))(parser);
    };
};
module.exports = {
    getField: getField,
    getFieldOptional: getFieldOptional,
    "getFieldOptional'": getFieldOptional$prime,
    defaultField: defaultField
};
