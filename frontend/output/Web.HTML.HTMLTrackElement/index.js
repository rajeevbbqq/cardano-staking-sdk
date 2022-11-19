// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Enum = require("../Data.Enum/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Effect = require("../Effect/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var Web_HTML_HTMLTrackElement_ReadyState = require("../Web.HTML.HTMLTrackElement.ReadyState/index.js");
var Web_Internal_FFI = require("../Web.Internal.FFI/index.js");
var toParentNode = Unsafe_Coerce.unsafeCoerce;
var toNonDocumentTypeChildNode = Unsafe_Coerce.unsafeCoerce;
var toNode = Unsafe_Coerce.unsafeCoerce;
var toHTMLElement = Unsafe_Coerce.unsafeCoerce;
var toEventTarget = Unsafe_Coerce.unsafeCoerce;
var toElement = Unsafe_Coerce.unsafeCoerce;
var toChildNode = Unsafe_Coerce.unsafeCoerce;
var readyState = function (el) {
    return Data_Functor.map(Effect.functorEffect)((function () {
        var $0 = Data_Maybe.fromMaybe(Web_HTML_HTMLTrackElement_ReadyState.None.value);
        var $1 = Data_Enum.toEnum(Web_HTML_HTMLTrackElement_ReadyState.boundedEnumReadyState);
        return function ($2) {
            return $0($1($2));
        };
    })())(function () {
        return $foreign["_readyState"](el);
    });
};
var fromParentNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
var fromNonDocumentTypeChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
var fromNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
var fromHTMLElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
var fromEventTarget = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
var fromElement = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
var fromChildNode = Web_Internal_FFI.unsafeReadProtoTagged("HTMLTrackElement");
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
    readyState: readyState,
    kind: $foreign.kind,
    setKind: $foreign.setKind,
    src: $foreign.src,
    setSrc: $foreign.setSrc,
    srclang: $foreign.srclang,
    setSrclang: $foreign.setSrclang,
    label: $foreign.label,
    setLabel: $foreign.setLabel,
    "default": $foreign["default"],
    setDefault: $foreign.setDefault
};
