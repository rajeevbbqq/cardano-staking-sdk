// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Text_Parsing_Parser = require("../Text.Parsing.Parser/index.js");
var Text_Parsing_Parser_String = require("../Text.Parsing.Parser.String/index.js");
var Text_Parsing_Parser_Token = require("../Text.Parsing.Parser.Token/index.js");
var emptyDef = (function () {
    var op$prime = function (dictMonad) {
        return Text_Parsing_Parser_String.oneOf(dictMonad)([ ":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~" ]);
    };
    return {
        commentStart: "",
        commentEnd: "",
        commentLine: "",
        nestedComments: true,
        identStart: Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.letter(Data_Identity.monadIdentity))(Text_Parsing_Parser_String["char"](Data_Identity.monadIdentity)("_")),
        identLetter: Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.alphaNum(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.oneOf(Data_Identity.monadIdentity)([ "_", "'" ])),
        opStart: op$prime(Data_Identity.monadIdentity),
        opLetter: op$prime(Data_Identity.monadIdentity),
        reservedOpNames: [  ],
        reservedNames: [  ],
        caseSensitive: true
    };
})();
var haskellStyle = (function () {
    var op$prime = function (dictMonad) {
        return Text_Parsing_Parser_String.oneOf(dictMonad)([ ":", "!", "#", "$", "%", "&", "*", "+", ".", "/", "<", "=", ">", "?", "@", "\\", "^", "|", "-", "~" ]);
    };
    var v = Text_Parsing_Parser_Token.unGenLanguageDef(emptyDef);
    return {
        commentStart: "{-",
        commentEnd: "-}",
        commentLine: "--",
        nestedComments: true,
        identStart: Text_Parsing_Parser_Token.letter(Data_Identity.monadIdentity),
        identLetter: Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.alphaNum(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.oneOf(Data_Identity.monadIdentity)([ "_", "'" ])),
        opStart: op$prime(Data_Identity.monadIdentity),
        opLetter: op$prime(Data_Identity.monadIdentity),
        reservedNames: [  ],
        reservedOpNames: [  ],
        caseSensitive: true
    };
})();
var haskell98Def = (function () {
    var v = Text_Parsing_Parser_Token.unGenLanguageDef(haskellStyle);
    return {
        commentStart: v.commentStart,
        commentEnd: v.commentEnd,
        commentLine: v.commentLine,
        nestedComments: v.nestedComments,
        identStart: v.identStart,
        identLetter: v.identLetter,
        opStart: v.opStart,
        opLetter: v.opLetter,
        reservedNames: [ "let", "in", "case", "of", "if", "then", "else", "data", "type", "class", "default", "deriving", "do", "import", "infix", "infixl", "infixr", "instance", "module", "newtype", "where", "primitive" ],
        reservedOpNames: [ "::", "..", "=", "\\", "|", "<-", "->", "@", "~", "=>" ],
        caseSensitive: v.caseSensitive
    };
})();
var haskellDef = {
    commentStart: haskell98Def.commentStart,
    commentEnd: haskell98Def.commentEnd,
    commentLine: haskell98Def.commentLine,
    nestedComments: haskell98Def.nestedComments,
    identStart: haskell98Def.identStart,
    identLetter: Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(haskell98Def.identLetter)(Text_Parsing_Parser_String["char"](Data_Identity.monadIdentity)("#")),
    opStart: haskell98Def.opStart,
    opLetter: haskell98Def.opLetter,
    reservedNames: Data_Semigroup.append(Data_Semigroup.semigroupArray)(haskell98Def.reservedNames)([ "foreign", "import", "export", "primitive", "_ccall_", "_casm_", "forall" ]),
    reservedOpNames: haskell98Def.reservedOpNames,
    caseSensitive: haskell98Def.caseSensitive
};
var haskell = Text_Parsing_Parser_Token.makeTokenParser(Data_Identity.monadIdentity)(haskellDef);
var javaStyle = (function () {
    var v = Text_Parsing_Parser_Token.unGenLanguageDef(emptyDef);
    return {
        commentStart: "/*",
        commentEnd: "*/",
        commentLine: "//",
        nestedComments: true,
        identStart: Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.letter(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.oneOf(Data_Identity.monadIdentity)([ "_", "$" ])),
        identLetter: Control_Alt.alt(Text_Parsing_Parser.altParserT(Data_Identity.monadIdentity))(Text_Parsing_Parser_Token.alphaNum(Data_Identity.monadIdentity))(Text_Parsing_Parser_String.oneOf(Data_Identity.monadIdentity)([ "_", "$" ])),
        opStart: v.opStart,
        opLetter: v.opLetter,
        reservedNames: [  ],
        reservedOpNames: [  ],
        caseSensitive: false
    };
})();
module.exports = {
    haskellDef: haskellDef,
    haskell: haskell,
    emptyDef: emptyDef,
    haskellStyle: haskellStyle,
    javaStyle: javaStyle
};
