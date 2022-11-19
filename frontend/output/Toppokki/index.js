// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Promise = require("../Control.Promise/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Uncurried = require("../Effect.Uncurried/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var URL = function (x) {
    return x;
};
var Selector = function (x) {
    return x;
};
var KeyboardKey = function (x) {
    return x;
};
var runPromiseAffE4 = function (f) {
    return function (a) {
        return function (b) {
            return function (c) {
                return function (d) {
                    return Control_Promise.toAffE(f(a, b, c, d));
                };
            };
        };
    };
};
var type_ = function (dictUnion) {
    return runPromiseAffE4($foreign["_type"]);
};
var runPromiseAffE3 = function (f) {
    return function (a) {
        return function (b) {
            return function (c) {
                return Control_Promise.toAffE(f(a, b, c));
            };
        };
    };
};
var select = runPromiseAffE3($foreign["_select"]);
var unsafePageEval = runPromiseAffE3($foreign["_unsafePageEval"]);
var unsafePageEvalAll = runPromiseAffE3($foreign["_unsafePageEvalAll"]);
var runPromiseAffE2 = function (f) {
    return function (a) {
        return function (b) {
            return Control_Promise.toAffE(f(a, b));
        };
    };
};
var screenshot = function (dictUnion) {
    return function (o) {
        return function (p) {
            return runPromiseAffE2($foreign["_screenshot"])(o)(p);
        };
    };
};
var setUserAgent = runPromiseAffE2($foreign["_setUserAgent"]);
var setViewport = runPromiseAffE2($foreign["_setViewport"]);
var unsafeEvaluateOnNewDocument = runPromiseAffE2($foreign["_unsafeEvaluateOnNewDocument"]);
var unsafeEvaluateStringFunction = runPromiseAffE2($foreign["_unsafeEvaluateStringFunction"]);
var waitForNavigation = function (dictUnion) {
    return runPromiseAffE2($foreign["_waitForNavigation"]);
};
var runPromiseAffE1 = function (f) {
    return function (a) {
        return Control_Promise.toAffE(f(a));
    };
};
var pdf = function (dictUnion) {
    return runPromiseAffE2($foreign["_pdf"]);
};
var pages = runPromiseAffE1($foreign["_pages"]);
var pageWaitForSelector = function (dictUnion) {
    return runPromiseAffE3($foreign["_pageWaitForSelector"]);
};
var onRequestFailed = Effect_Uncurried.runEffectFn3($foreign["_on"])("requestfailed");
var onPageError = Effect_Uncurried.runEffectFn3($foreign["_on"])("pageerror");
var onLoad = Effect_Uncurried.runEffectFn3($foreign["_on"])("load");
var onConsole = Effect_Uncurried.runEffectFn3($foreign["_on"])("console");
var newtypeURL = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeSelector = {
    Coercible0: function () {
        return undefined;
    }
};
var newPage = runPromiseAffE1($foreign["_newPage"]);
var networkIdle2 = "networkidle2";
var networkIdle0 = "networkidle0";
var networkIdle = "networkidle";
var makePDFMargin = function (dictUnion) {
    return Unsafe_Coerce.unsafeCoerce;
};
var launchChromeAWS = function (dictUnion) {
    return runPromiseAffE2($foreign["_launchChromeAWS"]);
};
var launch = function (dictUnion) {
    return runPromiseAffE1($foreign["_launch"]);
};
var keyboardUp = function (dictUnion) {
    return runPromiseAffE3($foreign["_keyboardUp"]);
};
var keyboardType = function (dictUnion) {
    return runPromiseAffE3($foreign["_keyboardType"]);
};
var keyboardSendCharacter = runPromiseAffE2($foreign["_keyboardSendCharacter"]);
var keyboardPress = function (dictUnion) {
    return runPromiseAffE3($foreign["_keyboardPress"]);
};
var keyboardDown = function (dictUnion) {
    return runPromiseAffE3($foreign["_keyboardDown"]);
};
var $$goto = runPromiseAffE2($foreign["_goto"]);
var getLocationRef = function (p) {
    return Control_Promise.toAffE($foreign["_getLocationHref"](p));
};
var focus = runPromiseAffE2($foreign["_focus"]);
var content = runPromiseAffE1($foreign["_content"]);
var consoleMessageText = (function () {
    var $12 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
    return function ($13) {
        return $12($foreign["_consoleMessageText"]($13));
    };
})();
var close = runPromiseAffE1($foreign["_close"]);
var click = runPromiseAffE2($foreign["_click"]);
var bringToFront = runPromiseAffE1($foreign["_bringToFront"]);
var addScriptTag = runPromiseAffE2($foreign["_addScriptTag"]);
module.exports = {
    URL: URL,
    Selector: Selector,
    launch: launch,
    launchChromeAWS: launchChromeAWS,
    newPage: newPage,
    pages: pages,
    "goto": $$goto,
    close: close,
    content: content,
    screenshot: screenshot,
    makePDFMargin: makePDFMargin,
    pdf: pdf,
    onPageError: onPageError,
    onLoad: onLoad,
    onConsole: onConsole,
    onRequestFailed: onRequestFailed,
    pageWaitForSelector: pageWaitForSelector,
    focus: focus,
    select: select,
    type_: type_,
    click: click,
    networkIdle: networkIdle,
    networkIdle0: networkIdle0,
    networkIdle2: networkIdle2,
    waitForNavigation: waitForNavigation,
    getLocationRef: getLocationRef,
    unsafeEvaluateOnNewDocument: unsafeEvaluateOnNewDocument,
    unsafeEvaluateStringFunction: unsafeEvaluateStringFunction,
    unsafePageEval: unsafePageEval,
    unsafePageEvalAll: unsafePageEvalAll,
    runPromiseAffE1: runPromiseAffE1,
    runPromiseAffE2: runPromiseAffE2,
    runPromiseAffE3: runPromiseAffE3,
    runPromiseAffE4: runPromiseAffE4,
    KeyboardKey: KeyboardKey,
    keyboardDown: keyboardDown,
    keyboardPress: keyboardPress,
    keyboardSendCharacter: keyboardSendCharacter,
    keyboardType: keyboardType,
    keyboardUp: keyboardUp,
    setViewport: setViewport,
    setUserAgent: setUserAgent,
    bringToFront: bringToFront,
    addScriptTag: addScriptTag,
    consoleMessageText: consoleMessageText,
    newtypeURL: newtypeURL,
    newtypeSelector: newtypeSelector,
    puppeteer: $foreign.puppeteer,
    "_launch": $foreign["_launch"],
    "_launchChromeAWS": $foreign["_launchChromeAWS"],
    "_newPage": $foreign["_newPage"],
    "_pages": $foreign["_pages"],
    "_goto": $foreign["_goto"],
    "_close": $foreign["_close"],
    "_content": $foreign["_content"],
    "_screenshot": $foreign["_screenshot"],
    "_pdf": $foreign["_pdf"],
    "_on": $foreign["_on"],
    "_pageWaitForSelector": $foreign["_pageWaitForSelector"],
    "_select": $foreign["_select"],
    "_focus": $foreign["_focus"],
    "_setViewport": $foreign["_setViewport"],
    "_type": $foreign["_type"],
    "_click": $foreign["_click"],
    "_waitForNavigation": $foreign["_waitForNavigation"],
    "_getLocationHref": $foreign["_getLocationHref"],
    "_unsafeEvaluateOnNewDocument": $foreign["_unsafeEvaluateOnNewDocument"],
    "_unsafeEvaluateStringFunction": $foreign["_unsafeEvaluateStringFunction"],
    "_unsafePageEval": $foreign["_unsafePageEval"],
    "_unsafePageEvalAll": $foreign["_unsafePageEvalAll"],
    "_keyboardDown": $foreign["_keyboardDown"],
    "_keyboardPress": $foreign["_keyboardPress"],
    "_keyboardSendCharacter": $foreign["_keyboardSendCharacter"],
    "_keyboardType": $foreign["_keyboardType"],
    "_keyboardUp": $foreign["_keyboardUp"],
    "_setUserAgent": $foreign["_setUserAgent"],
    "_bringToFront": $foreign["_bringToFront"],
    "_addScriptTag": $foreign["_addScriptTag"],
    "_consoleMessageText": $foreign["_consoleMessageText"]
};