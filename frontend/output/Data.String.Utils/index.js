// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_CodePoints = require("../Data.String.CodePoints/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Data_String_Common = require("../Data.String.Common/index.js");
var Data_String_Regex = require("../Data.String.Regex/index.js");
var Data_String_Regex_Flags = require("../Data.String.Regex.Flags/index.js");
var Data_String_Regex_Unsafe = require("../Data.String.Regex.Unsafe/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var NFC = (function () {
    function NFC() {

    };
    NFC.value = new NFC();
    return NFC;
})();
var NFD = (function () {
    function NFD() {

    };
    NFD.value = new NFD();
    return NFD;
})();
var NFKC = (function () {
    function NFKC() {

    };
    NFKC.value = new NFKC();
    return NFKC;
})();
var NFKD = (function () {
    function NFKD() {

    };
    NFKD.value = new NFKD();
    return NFKD;
})();
var words = function (s) {
    return $foreign.wordsImpl(s);
};
var unsafeRepeat = function (n) {
    return function (s) {
        return $foreign.unsafeRepeatImpl(n, s);
    };
};
var unsafeCodePointAt$prime = function (n) {
    return function (s) {
        return $foreign.unsafeCodePointAtPrimeImpl(n, s);
    };
};
var unsafeCodePointAt = function (n) {
    return function (s) {
        return $foreign.unsafeCodePointAtImpl(n, s);
    };
};
var trimStart = function (s) {
    return s.trimStart(Data_Unit.unit);
};
var trimEnd = function (s) {
    return s.trimEnd(Data_Unit.unit);
};
var toCharArray = function (s) {
    return $foreign.toCharArrayImpl(s);
};
var stripDiacritics = function (s) {
    return $foreign.stripDiacriticsImpl(s);
};
var stripChars = function (chars) {
    return function (s) {
        return $foreign.stripCharsImpl(chars, s);
    };
};
var startsWith$prime = function (searchString) {
    return function (position) {
        return function (s) {
            return $foreign.startsWithPrimeImpl(searchString, position, s);
        };
    };
};
var startsWith = function (searchString) {
    return function (s) {
        return $foreign.startsWithImpl(searchString, s);
    };
};
var showNormalizationForm = {
    show: function (v) {
        if (v instanceof NFC) {
            return "NFC";
        };
        if (v instanceof NFD) {
            return "NFD";
        };
        if (v instanceof NFKC) {
            return "NFKC";
        };
        if (v instanceof NFKD) {
            return "NFKD";
        };
        throw new Error("Failed pattern match at Data.String.Utils (line 264, column 1 - line 268, column 21): " + [ v.constructor.name ]);
    }
};
var repeat = function (n) {
    return function (s) {
        return $foreign.repeatImpl(Data_Maybe.Just.create, Data_Maybe.Nothing.value, n, s);
    };
};
var padStart$prime = function (n) {
    return function (s) {
        return $foreign.padStartPrimeImpl(n, s);
    };
};
var padStart = function (n) {
    return function (s) {
        var numberOfCodeUnits = Data_String_CodeUnits.length(s);
        var numberOfCodePoints = Data_String_CodePoints.length(s);
        return padStart$prime((n + numberOfCodeUnits | 0) - numberOfCodePoints | 0)(s);
    };
};
var padEnd$prime = function (n) {
    return function (s) {
        return $foreign.padEndPrimeImpl(n, s);
    };
};
var padEnd = function (n) {
    return function (s) {
        var numberOfCodeUnits = Data_String_CodeUnits.length(s);
        var numberOfCodePoints = Data_String_CodePoints.length(s);
        return padEnd$prime((n + numberOfCodeUnits | 0) - numberOfCodePoints | 0)(s);
    };
};
var normalize$prime = function (nf) {
    return function (s) {
        return $foreign.normalizePrimeImpl(Data_Show.show(showNormalizationForm)(nf), s);
    };
};
var normalize = function (s) {
    return $foreign.normalizeImpl(s);
};
var lines = function (s) {
    return $foreign.linesImpl(s);
};
var stripMarginWith = function (delimiter) {
    var go = function (line) {
        var trimmed = trimStart(line);
        var $5 = startsWith(delimiter)(trimmed);
        if ($5) {
            return Data_String_CodePoints.drop(Data_String_CodePoints.length(delimiter))(trimmed);
        };
        return line;
    };
    var $6 = Data_String_Common.joinWith("\x0a");
    var $7 = Data_Functor.map(Data_Functor.functorArray)(go);
    return function ($8) {
        return $6($7(lines(Data_String_Common.trim($8))));
    };
};
var stripMargin = stripMarginWith("|");
var length = function (dictWarn) {
    return function (s) {
        return $foreign.lengthImpl(s);
    };
};
var includes$prime = function (needle) {
    return function (position) {
        return function (haystack) {
            return $foreign.includesPrimeImpl(needle, position, haystack);
        };
    };
};
var includes = function (searchString) {
    return function (s) {
        return $foreign.includesImpl(searchString, s);
    };
};
var fromCharArray = function (arr) {
    return $foreign.fromCharArrayImpl(arr);
};
var mapChars = function (f) {
    var $9 = Data_Functor.map(Data_Functor.functorArray)(f);
    return function ($10) {
        return fromCharArray($9(toCharArray($10)));
    };
};
var filter = function (p) {
    var $11 = Data_Array.filter(p);
    return function ($12) {
        return fromCharArray($11(toCharArray($12)));
    };
};
var escapeRegex = function (s) {
    return $foreign.escapeRegexImpl(s);
};
var replaceAll = function (dictWarn) {
    var mkRegex = function (str) {
        return Data_String_Regex_Unsafe.unsafeRegex(escapeRegex(str))(Data_String_Regex_Flags.global);
    };
    return function ($13) {
        return Data_String_Regex.replace(mkRegex($13));
    };
};
var endsWith$prime = function (searchString) {
    return function (position) {
        return function (s) {
            return $foreign.endsWithPrimeImpl(searchString, position, s);
        };
    };
};
var endsWith = function (searchString) {
    return function (s) {
        return $foreign.endsWithImpl(searchString, s);
    };
};
var codePointAt$prime = function (n) {
    return function (s) {
        return $foreign.codePointAtPrimeImpl(Data_Maybe.Just.create, Data_Maybe.Nothing.value, n, s);
    };
};
var codePointAt = function (dictWarn) {
    return function (n) {
        return function (s) {
            return $foreign.codePointAtImpl(Data_Maybe.Just.create, Data_Maybe.Nothing.value, n, s);
        };
    };
};
var charAt = function (n) {
    return function (str) {
        return Data_Array.index(toCharArray(str))(n);
    };
};
module.exports = {
    NFC: NFC,
    NFD: NFD,
    NFKC: NFKC,
    NFKD: NFKD,
    charAt: charAt,
    codePointAt: codePointAt,
    "codePointAt'": codePointAt$prime,
    endsWith: endsWith,
    "endsWith'": endsWith$prime,
    escapeRegex: escapeRegex,
    filter: filter,
    fromCharArray: fromCharArray,
    includes: includes,
    "includes'": includes$prime,
    length: length,
    lines: lines,
    mapChars: mapChars,
    normalize: normalize,
    "normalize'": normalize$prime,
    padEnd: padEnd,
    "padEnd'": padEnd$prime,
    padStart: padStart,
    "padStart'": padStart$prime,
    repeat: repeat,
    replaceAll: replaceAll,
    startsWith: startsWith,
    "startsWith'": startsWith$prime,
    stripChars: stripChars,
    stripDiacritics: stripDiacritics,
    stripMargin: stripMargin,
    stripMarginWith: stripMarginWith,
    toCharArray: toCharArray,
    trimEnd: trimEnd,
    trimStart: trimStart,
    unsafeCodePointAt: unsafeCodePointAt,
    "unsafeCodePointAt'": unsafeCodePointAt$prime,
    unsafeRepeat: unsafeRepeat,
    words: words,
    showNormalizationForm: showNormalizationForm
};
