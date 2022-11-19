// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Nullable = require("../Data.Nullable/index.js");
var Effect = require("../Effect/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toHTMLElement = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toElement = Unsafe_Coerce.unsafeCoerce;
var toChildNode = Unsafe_Coerce.unsafeCoerce;
var tHead = (function () {
    var $0 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($1) {
        return $0($foreign["_tHead"]($1));
    };
})();
var tFoot = (function () {
    var $2 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($3) {
        return $2($foreign["_tFoot"]($3));
    };
})();
var setTHead = function ($4) {
    return $foreign["_setTHead"](Data_Nullable.toNullable($4));
};
var setTFoot = function ($5) {
    return $foreign["_setTFoot"](Data_Nullable.toNullable($5));
};
var setCaption = function ($6) {
    return $foreign["_setCaption"](Data_Nullable.toNullable($6));
};
var insertRow$prime = $foreign.insertRowAt;
var insertRow = insertRow$prime(-1 | 0);
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var fromNonDocumentTypeChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var fromHTMLElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var fromChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTableElement");
var caption = (function () {
    var $7 = Data_Functor.map(Effect.functorEffect)(Data_Nullable.toMaybe);
    return function ($8) {
        return $7($foreign["_caption"]($8));
    };
})();
module.exports = {
    fromHTMLElement: fromHTMLElement,
    fromElement: fromElement,
    fromNode: fromNode,
    fromChildNode: fromChildNode,
    fromNonDocumentTypeChildNode: fromNonDocumentTypeChildNode,
    fromParentNode: fromParentNode,
    fromEventTarget: fromEventTarget,
    toHTMLElement: toHTMLElement,
    toElement: toElement,
    toNode: toNode,
    toChildNode: toChildNode,
    toNonDocumentTypeChildNode: toNonDocumentTypeChildNode,
    toParentNode: toParentNode,
    toEventTarget: toEventTarget,
    caption: caption,
    setCaption: setCaption,
    tHead: tHead,
    setTHead: setTHead,
    tFoot: tFoot,
    setTFoot: setTFoot,
    insertRow: insertRow,
    "insertRow'": insertRow$prime,
    createCaption: $foreign.createCaption,
    deleteCaption: $foreign.deleteCaption,
    createTHead: $foreign.createTHead,
    deleteTHead: $foreign.deleteTHead,
    createTFoot: $foreign.createTFoot,
    deleteTFoot: $foreign.deleteTFoot,
    tBodies: $foreign.tBodies,
    createTBody: $foreign.createTBody,
    rows: $foreign.rows,
    deleteRow: $foreign.deleteRow,
    border: $foreign.border,
    setBorder: $foreign.setBorder
};
