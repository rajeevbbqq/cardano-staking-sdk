// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Cardano_Types_NativeScript = require("../Cardano.Types.NativeScript/index.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var convertTimelockStart = (function () {
    var $0 = Data_Functor.map(Data_Maybe.functorMaybe)(function ($3) {
        return Cardano_Types_NativeScript.TimelockStart.create(Serialization_Address.Slot($foreign.timelockStart_slot($3)));
    });
    var $1 = $foreign.getTimelockStart(FfiHelpers.maybeFfiHelper);
    return function ($2) {
        return $0($1($2));
    };
})();
var convertTimelockExpiry = (function () {
    var $4 = Data_Functor.map(Data_Maybe.functorMaybe)(function ($7) {
        return Cardano_Types_NativeScript.TimelockExpiry.create(Serialization_Address.Slot($foreign.timelockExpiry_slot($7)));
    });
    var $5 = $foreign.getTimelockExpiry(FfiHelpers.maybeFfiHelper);
    return function ($6) {
        return $4($5($6));
    };
})();
var convertScriptPubKey = function (ns) {
    return Data_Functor.map(Data_Maybe.functorMaybe)(function ($8) {
        return Cardano_Types_NativeScript.ScriptPubkey.create($foreign.scriptPubkey_addr_keyhash($8));
    })($foreign.getScriptPubkey(FfiHelpers.maybeFfiHelper)(ns));
};
var convertScriptNOfK = function (ns) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)($foreign.getScriptNOfK(FfiHelpers.maybeFfiHelper)(ns))(function (scriptNOfK) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertNativeScript)($foreign.scriptNOfKScripts(FfiHelpers.containerHelper)(scriptNOfK)))(function (res) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Cardano_Types_NativeScript.ScriptNOfK($foreign.scriptNOfK_n(scriptNOfK), res));
        });
    });
};
var convertScriptAny = function (ns) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)($foreign.getScriptAny(FfiHelpers.maybeFfiHelper)(ns))(function (scriptAny) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(Cardano_Types_NativeScript.ScriptAny.create)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertNativeScript)($foreign.scriptAnyScripts(FfiHelpers.containerHelper)(scriptAny)));
    });
};
var convertScriptAll = function (ns) {
    return Control_Bind.bind(Data_Maybe.bindMaybe)($foreign.getScriptAll(FfiHelpers.maybeFfiHelper)(ns))(function (scriptAll) {
        return Data_Functor.map(Data_Maybe.functorMaybe)(Cardano_Types_NativeScript.ScriptAll.create)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(convertNativeScript)($foreign.scriptAllScripts(FfiHelpers.containerHelper)(scriptAll)));
    });
};
var convertNativeScript = function (ns) {
    return Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(Control_Alt.alt(Data_Maybe.altMaybe)(convertScriptPubKey(ns))(convertScriptAll(ns)))(convertScriptAny(ns)))(convertScriptNOfK(ns)))(convertTimelockStart(ns)))(convertTimelockExpiry(ns));
};
module.exports = {
    convertNativeScript: convertNativeScript
};