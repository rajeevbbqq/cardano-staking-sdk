// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Text_Parsing_Parser = require("../Text.Parsing.Parser/index.js");
var Text_Parsing_Parser_Combinators = require("../Text.Parsing.Parser.Combinators/index.js");
var AssocNone = (function () {
    function AssocNone() {

    };
    AssocNone.value = new AssocNone();
    return AssocNone;
})();
var AssocLeft = (function () {
    function AssocLeft() {

    };
    AssocLeft.value = new AssocLeft();
    return AssocLeft;
})();
var AssocRight = (function () {
    function AssocRight() {

    };
    AssocRight.value = new AssocRight();
    return AssocRight;
})();
var Infix = (function () {
    function Infix(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Infix.create = function (value0) {
        return function (value1) {
            return new Infix(value0, value1);
        };
    };
    return Infix;
})();
var Prefix = (function () {
    function Prefix(value0) {
        this.value0 = value0;
    };
    Prefix.create = function (value0) {
        return new Prefix(value0);
    };
    return Prefix;
})();
var Postfix = (function () {
    function Postfix(value0) {
        this.value0 = value0;
    };
    Postfix.create = function (value0) {
        return new Postfix(value0);
    };
    return Postfix;
})();
var termP = function (dictMonad) {
    return function (prefixP) {
        return function (term) {
            return function (postfixP) {
                return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(prefixP)(function (pre) {
                    return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(term)(function (x) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(postfixP)(function (post) {
                            return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(post(pre(x)));
                        });
                    });
                });
            };
        };
    };
};
var splitOp = function (v) {
    return function (accum) {
        if (v instanceof Infix && v.value1 instanceof AssocNone) {
            return {
                rassoc: accum.rassoc,
                lassoc: accum.lassoc,
                nassoc: new Data_List_Types.Cons(v.value0, accum.nassoc),
                prefix: accum.prefix,
                postfix: accum.postfix
            };
        };
        if (v instanceof Infix && v.value1 instanceof AssocLeft) {
            return {
                rassoc: accum.rassoc,
                lassoc: new Data_List_Types.Cons(v.value0, accum.lassoc),
                nassoc: accum.nassoc,
                prefix: accum.prefix,
                postfix: accum.postfix
            };
        };
        if (v instanceof Infix && v.value1 instanceof AssocRight) {
            return {
                rassoc: new Data_List_Types.Cons(v.value0, accum.rassoc),
                lassoc: accum.lassoc,
                nassoc: accum.nassoc,
                prefix: accum.prefix,
                postfix: accum.postfix
            };
        };
        if (v instanceof Prefix) {
            return {
                rassoc: accum.rassoc,
                lassoc: accum.lassoc,
                nassoc: accum.nassoc,
                prefix: new Data_List_Types.Cons(v.value0, accum.prefix),
                postfix: accum.postfix
            };
        };
        if (v instanceof Postfix) {
            return {
                rassoc: accum.rassoc,
                lassoc: accum.lassoc,
                nassoc: accum.nassoc,
                prefix: accum.prefix,
                postfix: new Data_List_Types.Cons(v.value0, accum.postfix)
            };
        };
        throw new Error("Failed pattern match at Text.Parsing.Parser.Expr (line 69, column 1 - line 69, column 80): " + [ v.constructor.name, accum.constructor.name ]);
    };
};
var rassocP1 = function (dictMonad) {
    return function (x) {
        return function (rassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(rassocP(dictMonad)(x)(rassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(x));
                    };
                };
            };
        };
    };
};
var rassocP = function (dictMonad) {
    return function (x) {
        return function (rassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(rassocOp)(function (f) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (z) {
                                return rassocP1(dictMonad)(z)(rassocOp)(prefixP)(term)(postfixP);
                            }))(function (y) {
                                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(f(x)(y));
                            });
                        });
                    };
                };
            };
        };
    };
};
var nassocP = function (dictMonad) {
    return function (x) {
        return function (nassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(nassocOp)(function (f) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (y) {
                                return Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(f(x)(y));
                            });
                        });
                    };
                };
            };
        };
    };
};
var lassocP1 = function (dictMonad) {
    return function (x) {
        return function (lassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(lassocP(dictMonad)(x)(lassocOp)(prefixP)(term)(postfixP))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(x));
                    };
                };
            };
        };
    };
};
var lassocP = function (dictMonad) {
    return function (x) {
        return function (lassocOp) {
            return function (prefixP) {
                return function (term) {
                    return function (postfixP) {
                        return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(lassocOp)(function (f) {
                            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (y) {
                                return lassocP1(dictMonad)(f(x)(y))(lassocOp)(prefixP)(term)(postfixP);
                            });
                        });
                    };
                };
            };
        };
    };
};
var makeParser = function (dictMonad) {
    return function (term) {
        return function (ops) {
            var accum = Data_Foldable.foldr(Data_Foldable.foldableArray)(splitOp)({
                rassoc: Data_List_Types.Nil.value,
                lassoc: Data_List_Types.Nil.value,
                nassoc: Data_List_Types.Nil.value,
                prefix: Data_List_Types.Nil.value,
                postfix: Data_List_Types.Nil.value
            })(ops);
            var lassocOp = Text_Parsing_Parser_Combinators.choice(Data_List_Types.foldableList)(dictMonad)(accum.lassoc);
            var nassocOp = Text_Parsing_Parser_Combinators.choice(Data_List_Types.foldableList)(dictMonad)(accum.nassoc);
            var postfixOp = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.choice(Data_List_Types.foldableList)(dictMonad)(accum.postfix))("");
            var postfixP = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(postfixOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Control_Category.identity(Control_Category.categoryFn)));
            var prefixOp = Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Text_Parsing_Parser_Combinators.choice(Data_List_Types.foldableList)(dictMonad)(accum.prefix))("");
            var prefixP = Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(prefixOp)(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(Control_Category.identity(Control_Category.categoryFn)));
            var rassocOp = Text_Parsing_Parser_Combinators.choice(Data_List_Types.foldableList)(dictMonad)(accum.rassoc);
            return Control_Bind.bind(Text_Parsing_Parser.bindParserT(dictMonad))(termP(dictMonad)(prefixP)(term)(postfixP))(function (x) {
                return Text_Parsing_Parser_Combinators.withErrorMessage(dictMonad)(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(Control_Alt.alt(Text_Parsing_Parser.altParserT(dictMonad))(rassocP(dictMonad)(x)(rassocOp)(prefixP)(term)(postfixP))(lassocP(dictMonad)(x)(lassocOp)(prefixP)(term)(postfixP)))(nassocP(dictMonad)(x)(nassocOp)(prefixP)(term)(postfixP)))(Control_Applicative.pure(Text_Parsing_Parser.applicativeParserT(dictMonad))(x)))("operator");
            });
        };
    };
};
var buildExprParser = function (dictMonad) {
    return function (operators) {
        return function (simpleExpr) {
            return Data_Foldable.foldl(Data_Foldable.foldableArray)(makeParser(dictMonad))(simpleExpr)(operators);
        };
    };
};
module.exports = {
    AssocNone: AssocNone,
    AssocLeft: AssocLeft,
    AssocRight: AssocRight,
    Infix: Infix,
    Prefix: Prefix,
    Postfix: Postfix,
    buildExprParser: buildExprParser
};