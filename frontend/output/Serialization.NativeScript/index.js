// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Cardano_Types_NativeScript = require("../Cardano.Types.NativeScript/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var packNativeScripts = $foreign["_packNativeScripts"](FfiHelpers.containerHelper);
var convertTimelockStart = function (v) {
    return $foreign.nativeScript_new_timelock_start($foreign.mkTimelockStart(v));
};
var convertTimelockExpiry = function (v) {
    return $foreign.nativeScript_new_timelock_expiry($foreign.mkTimelockExpiry(v));
};
var convertScriptPubkey = function (hash) {
    return $foreign.nativeScript_new_script_pubkey($foreign.mkScriptPubkey(hash));
};
var convertScriptNOfK = function (n) {
    return function (nss) {
        return Data_Functor.map(Data_Maybe.functorMaybe)((function () {
            var $13 = $foreign.mkScriptNOfK(n);
            return function ($14) {
                return $foreign.nativeScript_new_script_n_of_k($13(packNativeScripts($14)));
            };
        })())(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(nss)(convertNativeScript));
    };
};
var convertScriptAny = function (nss) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function ($15) {
        return $foreign.nativeScript_new_script_any($foreign.mkScriptAny(packNativeScripts($15)));
    })(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(nss)(convertNativeScript));
};
var convertScriptAll = function (nss) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function ($16) {
        return $foreign.nativeScript_new_script_all($foreign.mkScriptAll(packNativeScripts($16)));
    })(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(nss)(convertNativeScript));
};
var convertNativeScript = function (v) {
    if (v instanceof Cardano_Types_NativeScript.ScriptPubkey) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(convertScriptPubkey(v.value0));
    };
    if (v instanceof Cardano_Types_NativeScript.ScriptAll) {
        return convertScriptAll(v.value0);
    };
    if (v instanceof Cardano_Types_NativeScript.ScriptAny) {
        return convertScriptAny(v.value0);
    };
    if (v instanceof Cardano_Types_NativeScript.ScriptNOfK) {
        return convertScriptNOfK(v.value0)(v.value1);
    };
    if (v instanceof Cardano_Types_NativeScript.TimelockStart) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(convertTimelockStart(v.value0));
    };
    if (v instanceof Cardano_Types_NativeScript.TimelockExpiry) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(convertTimelockExpiry(v.value0));
    };
    throw new Error("Failed pattern match at Serialization.NativeScript (line 40, column 23 - line 46, column 61): " + [ v.constructor.name ]);
};
var convertNativeScripts = (function () {
    var $17 = Data_Functor.map(Data_Maybe.functorMaybe)(packNativeScripts);
    var $18 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertNativeScript);
    return function ($19) {
        return $17($18($19));
    };
})();
module.exports = {
    convertNativeScript: convertNativeScript,
    convertNativeScripts: convertNativeScripts
};
