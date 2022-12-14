// Generated by purs version 0.14.5
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_String_CodePoints = require("../Data.String.CodePoints/index.js");
var Data_String_Common = require("../Data.String.Common/index.js");
var Data_String_Regex = require("../Data.String.Regex/index.js");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags/index.js");
var Partial_Unsafe = require("../Partial.Unsafe/index.js");
var whitespaceRegex = (function () {
    var v = Data_String_Regex.regex("\\s+")(Data_String_Regex_Flags.noFlags);
    if (v instanceof Data_Either.Left) {
        return Partial_Unsafe.unsafeCrashWith("whitespaceRegex: `\\s+` seems to be invlaid, err: " + v.value0);
    };
    if (v instanceof Data_Either.Right) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Options.Applicative.Internal.Utils (line 39, column 19 - line 41, column 15): " + [ v.constructor.name ]);
})();
var words = function (v) {
    if (v === "") {
        return [  ];
    };
    return Data_String_Regex.split(whitespaceRegex)(v);
};
var unWords = function (dictFoldable) {
    return Data_Foldable.intercalate(dictFoldable)(Data_Monoid.monoidString)(" ");
};
var unLines = function (dictFoldable) {
    return Data_Foldable.intercalate(dictFoldable)(Data_Monoid.monoidString)("\x0a");
};
var startsWith = function (p) {
    return function (s) {
        return Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqInt))(Data_String_CodePoints.indexOf(p)(s))(new Data_Maybe.Just(0));
    };
};
var lines = function (v) {
    if (v === "") {
        return [  ];
    };
    return Data_String_Common.split("\x0a")(v);
};
var apApplyFlipped = function (dictApply) {
    return Control_Apply.lift2(dictApply)(Data_Function.applyFlipped);
};
module.exports = {
    unLines: unLines,
    unWords: unWords,
    lines: lines,
    words: words,
    whitespaceRegex: whitespaceRegex,
    startsWith: startsWith,
    apApplyFlipped: apApplyFlipped
};
