// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Variant = require("../Data.Variant/index.js");
var Deserialization_Error = require("../Deserialization.Error/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var Types_Scripts = require("../Types.Scripts/index.js");
var convertLanguage = $foreign["_convertLanguage"](FfiHelpers.errorHelper(Data_Variant.inj()({
    reflectSymbol: function () {
        return "fromCslRepError";
    }
})(Deserialization_Error["_fromCslRepError"])))({
    plutusV1: Types_Scripts.PlutusV1.value,
    plutusV2: Types_Scripts.PlutusV2.value
});
module.exports = {
    convertLanguage: convertLanguage,
    "_convertLanguage": $foreign["_convertLanguage"]
};
