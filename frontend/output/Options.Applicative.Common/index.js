// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Free = require("../Control.Monad.Free/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Exists = require("../Data.Exists/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_List = require("../Data.List/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Options_Applicative_Internal = require("../Options.Applicative.Internal/index.js");
var Options_Applicative_Internal_Utils = require("../Options.Applicative.Internal.Utils/index.js");
var Options_Applicative_Types = require("../Options.Applicative.Types/index.js");
var OptWord = (function () {
    function OptWord(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    OptWord.create = function (value0) {
        return function (value1) {
            return new OptWord(value0, value1);
        };
    };
    return OptWord;
})();
var unexpectedError = function (arg) {
    return function (p) {
        return Options_Applicative_Types.UnexpectedError.create(arg)(Options_Applicative_Types.SomeParser.create(Data_Exists.mkExists(p)));
    };
};
var simplify = function (v) {
    if (v instanceof Options_Applicative_Types.Leaf) {
        return new Options_Applicative_Types.Leaf(v.value0);
    };
    if (v instanceof Options_Applicative_Types.MultNode) {
        var remove_mult = function (v1) {
            if (v1 instanceof Options_Applicative_Types.MultNode) {
                return v1.value0;
            };
            return [ v1 ];
        };
        var v1 = Control_Bind.bind(Control_Bind.bindArray)(v.value0)(function ($221) {
            return remove_mult(simplify($221));
        });
        if (v1.length === 1) {
            return v1[0];
        };
        return new Options_Applicative_Types.MultNode(v1);
    };
    if (v instanceof Options_Applicative_Types.AltNode) {
        var remove_alt = function (v1) {
            if (v1 instanceof Options_Applicative_Types.AltNode) {
                return v1.value0;
            };
            if (v1 instanceof Options_Applicative_Types.MultNode && v1.value0.length === 0) {
                return [  ];
            };
            return [ v1 ];
        };
        var v1 = Control_Bind.bind(Control_Bind.bindArray)(v.value0)(function ($222) {
            return remove_alt(simplify($222));
        });
        if (v1.length === 0) {
            return new Options_Applicative_Types.MultNode([  ]);
        };
        if (v1.length === 1) {
            return v1[0];
        };
        return new Options_Applicative_Types.AltNode(v1);
    };
    throw new Error("Failed pattern match at Options.Applicative.Common (line 279, column 1 - line 279, column 45): " + [ v.constructor.name ]);
};
var showOption = function (v) {
    if (v instanceof Options_Applicative_Types.OptLong) {
        return "--" + v.value0;
    };
    if (v instanceof Options_Applicative_Types.OptShort) {
        return Data_String_CodeUnits.fromCharArray([ "-", v.value0 ]);
    };
    throw new Error("Failed pattern match at Options.Applicative.Common (line 42, column 1 - line 42, column 32): " + [ v.constructor.name ]);
};
var parseWord = (function () {
    var go = function (v) {
        if (v instanceof Data_List_Types.Cons && (v.value0 === "-" && (v.value1 instanceof Data_List_Types.Cons && v.value1.value0 === "-"))) {
            return Data_Maybe.Just.create((function () {
                var v1 = (function () {
                    var v2 = Data_List.span(function (v3) {
                        return v3 !== "=";
                    })(v.value1.value1);
                    if (v2.rest instanceof Data_List_Types.Nil) {
                        return new Data_Tuple.Tuple(v.value1.value1, Data_Maybe.Nothing.value);
                    };
                    if (v2.rest instanceof Data_List_Types.Cons) {
                        return new Data_Tuple.Tuple(v2.init, new Data_Maybe.Just(v2.rest.value1));
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 106, column 23 - line 108, column 70): " + [ v2.constructor.name ]);
                })();
                return new OptWord(new Options_Applicative_Types.OptLong(Data_String_CodeUnits.fromCharArray(Data_Array.fromFoldable(Data_List_Types.foldableList)(v1.value0))), Data_Functor.map(Data_Maybe.functorMaybe)((function () {
                    var $223 = Data_Array.fromFoldable(Data_List_Types.foldableList);
                    return function ($224) {
                        return Data_String_CodeUnits.fromCharArray($223($224));
                    };
                })())(v1.value1));
            })());
        };
        if (v instanceof Data_List_Types.Cons && v.value0 === "-") {
            if (v.value1 instanceof Data_List_Types.Nil) {
                return Data_Maybe.Nothing.value;
            };
            if (v.value1 instanceof Data_List_Types.Cons) {
                return Data_Maybe.Just.create((function () {
                    var arg = Data_Functor.voidRight(Data_Maybe.functorMaybe)(v.value1.value1)(Control_Alternative.guard(Data_Maybe.alternativeMaybe)(!Data_List["null"](v.value1.value1)));
                    return new OptWord(new Options_Applicative_Types.OptShort(v.value1.value0), Data_Functor.map(Data_Maybe.functorMaybe)((function () {
                        var $225 = Data_Array.fromFoldable(Data_List_Types.foldableList);
                        return function ($226) {
                            return Data_String_CodeUnits.fromCharArray($225($226));
                        };
                    })())(arg));
                })());
            };
            throw new Error("Failed pattern match at Options.Applicative.Common (line 110, column 25 - line 114, column 79): " + [ v.value1.constructor.name ]);
        };
        return Data_Maybe.Nothing.value;
    };
    var $227 = Data_List.fromFoldable(Data_Foldable.foldableArray);
    return function ($228) {
        return go($227(Data_String_CodeUnits.toCharArray($228)));
    };
})();
var optionNames = function (v) {
    if (v instanceof Options_Applicative_Types.OptReader) {
        return v.value0;
    };
    if (v instanceof Options_Applicative_Types.FlagReader) {
        return v.value0;
    };
    return [  ];
};
var liftOpt = Options_Applicative_Types.OptP.create;
var isOptionPrefix = function (v) {
    return function (v1) {
        if (v instanceof Options_Applicative_Types.OptShort && v1 instanceof Options_Applicative_Types.OptShort) {
            return v.value0 === v1.value0;
        };
        if (v instanceof Options_Applicative_Types.OptLong && v1 instanceof Options_Applicative_Types.OptLong) {
            return Options_Applicative_Internal_Utils.startsWith(v.value0)(v1.value0);
        };
        return false;
    };
};
var optMatches = function (dictMonadP) {
    return function (disambiguate) {
        return function (opt) {
            return function (v) {
                var is_short = function (v1) {
                    if (v1 instanceof Options_Applicative_Types.OptShort) {
                        return true;
                    };
                    if (v1 instanceof Options_Applicative_Types.OptLong) {
                        return false;
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 89, column 5 - line 89, column 33): " + [ v1.constructor.name ]);
                };
                var has_name = function (a) {
                    if (disambiguate) {
                        return Data_Foldable.any(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(isOptionPrefix(a));
                    };
                    if (Data_Boolean.otherwise) {
                        return Data_Foldable.elem(Data_Foldable.foldableArray)(Options_Applicative_Types.optNameEq)(a);
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 92, column 5 - line 94, column 27): " + [ a.constructor.name ]);
                };
                var errorFor = function (name) {
                    return function (msg) {
                        return "option " + (showOption(name) + (": " + msg));
                    };
                };
                if (opt instanceof Options_Applicative_Types.OptReader) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_Alternative.guard(Data_Maybe.alternativeMaybe)(has_name(v.value0)(opt.value0)))(function () {
                        return Data_Maybe.Just.create(Control_Bind.bind(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(dictMonadP.Monad0())))(function (args) {
                            var missing_arg = Options_Applicative_Internal.missingArgP(dictMonadP)(opt.value2(showOption(v.value0)))((Data_Newtype.un()(Options_Applicative_Types.CReader)(opt.value1)).crCompleter);
                            return Control_Bind.bind(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))((function () {
                                var v1 = Data_Maybe.maybe(args)(function (v2) {
                                    return new Data_List_Types.Cons(v2, args);
                                })(v.value1);
                                if (v1 instanceof Data_List_Types.Nil) {
                                    return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonadP.Monad0())(missing_arg);
                                };
                                if (v1 instanceof Data_List_Types.Cons) {
                                    return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(dictMonadP.Monad0()))(new Data_Tuple.Tuple(v1.value0, v1.value1));
                                };
                                throw new Error("Failed pattern match at Options.Applicative.Common (line 67, column 27 - line 69, column 56): " + [ v1.constructor.name ]);
                            })())(function (v1) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))(Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(dictMonadP.Monad0()))(v1.value1))(function () {
                                    return Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonadP.Monad0())(Options_Applicative_Internal.runReadM(dictMonadP)(Options_Applicative_Internal.withReadM(errorFor(v.value0))((Data_Newtype.un()(Options_Applicative_Types.CReader)(opt.value1)).crReader))(v1.value0));
                                });
                            });
                        }));
                    });
                };
                if (opt instanceof Options_Applicative_Types.FlagReader) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_Alternative.guard(Data_Maybe.alternativeMaybe)(has_name(v.value0)(opt.value0)))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_Alternative.guard(Data_Maybe.alternativeMaybe)(is_short(v.value0) || Data_Maybe.isNothing(v.value1)))(function () {
                            return Data_Maybe.Just.create(Control_Bind.bind(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(dictMonadP.Monad0())))(function (args) {
                                var val$prime = Data_Functor.map(Data_Maybe.functorMaybe)(function ($229) {
                                    return (function (s) {
                                        return Data_Array.cons("-")(s);
                                    })(Data_String_CodeUnits.toCharArray($229));
                                })(v.value1);
                                return Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))(Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(dictMonadP.Monad0()))(Data_Maybe.maybe(args)((function () {
                                    var $230 = Data_Function.flip(Data_List_Types.Cons.create)(args);
                                    return function ($231) {
                                        return $230(Data_String_CodeUnits.fromCharArray($231));
                                    };
                                })())(val$prime)))(function () {
                                    return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(dictMonadP.Monad0()))(opt.value1);
                                });
                            }));
                        });
                    });
                };
                return Data_Maybe.Nothing.value;
            };
        };
    };
};
var isArg = function (v) {
    if (v instanceof Options_Applicative_Types.ArgReader) {
        return true;
    };
    return false;
};
var evalParser = function (v) {
    if (v instanceof Options_Applicative_Types.NilP) {
        return new Data_Maybe.Just(v.value0);
    };
    if (v instanceof Options_Applicative_Types.OptP) {
        return Data_Maybe.Nothing.value;
    };
    if (v instanceof Options_Applicative_Types.MultP) {
        return Data_Exists.runExists(function (v1) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(evalParser(v1.value0))(evalParser(v1.value1));
        })(v.value0);
    };
    if (v instanceof Options_Applicative_Types.AltP) {
        return Control_Alt.alt(Data_Maybe.altMaybe)(evalParser(v.value0))(evalParser(v.value1));
    };
    if (v instanceof Options_Applicative_Types.BindP) {
        return Control_Monad_Free["resume'"](function (p) {
            return function (k) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(evalParser(p))(function ($232) {
                    return evalParser(Options_Applicative_Types.BindP.create(k($232)));
                });
            };
        })(Data_Maybe.Just.create)(v.value0);
    };
    throw new Error("Failed pattern match at Options.Applicative.Common (line 219, column 1 - line 219, column 44): " + [ v.constructor.name ]);
};
var searchParser = function (dictMonad) {
    return function (v) {
        return function (v1) {
            if (v1 instanceof Options_Applicative_Types.NilP) {
                return Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(dictMonad));
            };
            if (v1 instanceof Options_Applicative_Types.OptP) {
                return v(v1.value0);
            };
            if (v1 instanceof Options_Applicative_Types.MultP) {
                return Data_Exists.runExists(function (v2) {
                    var b = Data_Functor.mapFlipped(Options_Applicative_Internal.nondetTFunctor(dictMonad))(searchParser(dictMonad)(v)(v2.value1))(function (p2$prime) {
                        return Control_Apply.apply(Options_Applicative_Types.parserApply)(v2.value0)(p2$prime);
                    });
                    var a = Data_Functor.mapFlipped(Options_Applicative_Internal.nondetTFunctor(dictMonad))(searchParser(dictMonad)(v)(v2.value0))(function (p1$prime) {
                        return Control_Apply.apply(Options_Applicative_Types.parserApply)(p1$prime)(v2.value1);
                    });
                    return Options_Applicative_Internal.nondetTAltOp(dictMonad)(a)(b);
                })(v1.value0);
            };
            if (v1 instanceof Options_Applicative_Types.AltP) {
                return Data_Foldable.oneOf(Data_Foldable.foldableArray)(Options_Applicative_Internal.nondetTPlus(dictMonad))([ searchParser(dictMonad)(v)(v1.value0), searchParser(dictMonad)(v)(v1.value1) ]);
            };
            if (v1 instanceof Options_Applicative_Types.BindP) {
                return Control_Monad_Free["resume'"](function (p) {
                    return function (k) {
                        return Data_Foldable.oneOf(Data_Foldable.foldableArray)(Options_Applicative_Internal.nondetTPlus(dictMonad))([ Data_Functor.mapFlipped(Options_Applicative_Internal.nondetTFunctor(dictMonad))(searchParser(dictMonad)(v)(p))(function (p$prime) {
                            return Options_Applicative_Types.BindP.create(Control_Bind.bind(Control_Monad_Free.freeBind)(Control_Monad_Free.liftF(p$prime))(k));
                        }), (function () {
                            var v2 = evalParser(p);
                            if (v2 instanceof Data_Maybe.Nothing) {
                                return Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(dictMonad));
                            };
                            if (v2 instanceof Data_Maybe.Just) {
                                return searchParser(dictMonad)(v)(Options_Applicative_Types.BindP.create(k(v2.value0)));
                            };
                            throw new Error("Failed pattern match at Options.Applicative.Common (line 134, column 7 - line 136, column 49): " + [ v2.constructor.name ]);
                        })() ]);
                    };
                })(Data_Function["const"](Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(dictMonad))))(v1.value0);
            };
            throw new Error("Failed pattern match at Options.Applicative.Common (line 117, column 1 - line 119, column 49): " + [ v.constructor.name, v1.constructor.name ]);
        };
    };
};
var searchOpt = function (dictMonadP) {
    return function (pprefs) {
        return function (w) {
            return searchParser(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(function (opt) {
                var disambiguate = (Data_Newtype.un()(Options_Applicative_Types.ParserPrefs)(pprefs)).prefDisambiguate && Data_Ord.greaterThan(Options_Applicative_Types.optVisibilityOrd)(Options_Applicative_Types.optVisibility(opt))(Options_Applicative_Types.Internal.value);
                var v = optMatches(dictMonadP)(disambiguate)((Data_Newtype.un()(Options_Applicative_Types.Option)(opt)).optMain)(w);
                if (v instanceof Data_Maybe.Just) {
                    return Control_Monad_Trans_Class.lift(Options_Applicative_Internal.nondetTMonadTrans)(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(Data_Functor.map(Control_Monad_State_Trans.functorStateT((dictMonadP.Alt1()).Functor0()))(Control_Applicative.pure(Options_Applicative_Types.parserApplicative))(v.value0));
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())));
                };
                throw new Error("Failed pattern match at Options.Applicative.Common (line 143, column 3 - line 145, column 21): " + [ v.constructor.name ]);
            });
        };
    };
};
var stepParser = function (dictMonadP) {
    return function (pprefs) {
        return function (v) {
            return function (arg) {
                return function (p) {
                    if (v instanceof Options_Applicative_Types.AllPositionals) {
                        return searchArg(dictMonadP)(pprefs)(arg)(p);
                    };
                    if (v instanceof Options_Applicative_Types.ForwardOptions) {
                        var v1 = parseWord(arg);
                        if (v1 instanceof Data_Maybe.Just) {
                            return Control_Alt.alt(Options_Applicative_Internal.nondetTAlt(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())))(searchOpt(dictMonadP)(pprefs)(v1.value0)(p))(searchArg(dictMonadP)(pprefs)(arg)(p));
                        };
                        if (v1 instanceof Data_Maybe.Nothing) {
                            return searchArg(dictMonadP)(pprefs)(arg)(p);
                        };
                        throw new Error("Failed pattern match at Options.Applicative.Common (line 173, column 42 - line 175, column 36): " + [ v1.constructor.name ]);
                    };
                    var v1 = parseWord(arg);
                    if (v1 instanceof Data_Maybe.Just) {
                        return searchOpt(dictMonadP)(pprefs)(v1.value0)(p);
                    };
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return searchArg(dictMonadP)(pprefs)(arg)(p);
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 176, column 29 - line 178, column 36): " + [ v1.constructor.name ]);
                };
            };
        };
    };
};
var searchArg = function (dictMonadP) {
    return function (prefs) {
        return function (arg) {
            return searchParser(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(function (opt) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Options_Applicative_Internal.nondetTBind(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())))(Control_Applicative.when(Options_Applicative_Internal.nondetTApplicative(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())))(isArg((Data_Newtype.un()(Options_Applicative_Types.Option)(opt)).optMain))(Options_Applicative_Internal.cut(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))))(function () {
                    var v = (Data_Newtype.un()(Options_Applicative_Types.Option)(opt)).optMain;
                    if (v instanceof Options_Applicative_Types.CmdReader) {
                        var v1 = new Data_Tuple.Tuple(v.value2(arg), (Data_Newtype.un()(Options_Applicative_Types.ParserPrefs)(prefs)).prefBacktrack);
                        if (v1.value0 instanceof Data_Maybe.Just && v1.value1 instanceof Options_Applicative_Types.NoBacktrack) {
                            return Control_Monad_Trans_Class.lift(Options_Applicative_Internal.nondetTMonadTrans)(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(Control_Bind.bind(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))(Control_Apply.applyFirst(Control_Monad_State_Trans.applyStateT(dictMonadP.Monad0()))(Control_Monad_State_Class.get(Control_Monad_State_Trans.monadStateStateT(dictMonadP.Monad0())))(Control_Monad_State_Class.put(Control_Monad_State_Trans.monadStateStateT(dictMonadP.Monad0()))(Data_List_Types.Nil.value)))(function (args) {
                                return Data_Functor.map(Control_Monad_State_Trans.functorStateT((dictMonadP.Alt1()).Functor0()))(Control_Applicative.pure(Options_Applicative_Types.parserApplicative))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonadP.Monad0())(Control_Apply.applyFirst(((dictMonadP.Monad0()).Bind1()).Apply0())(Control_Apply.applySecond(((dictMonadP.Monad0()).Bind1()).Apply0())(Options_Applicative_Internal.enterContext(dictMonadP)(arg)(v1.value0.value0))(runParserInfo(dictMonadP)(v1.value0.value0)(args)))(Options_Applicative_Internal.exitContext(dictMonadP))));
                            }));
                        };
                        if (v1.value0 instanceof Data_Maybe.Just && v1.value1 instanceof Options_Applicative_Types.Backtrack) {
                            return Data_Functor.map(Options_Applicative_Internal.nondetTFunctor(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())))(Control_Applicative.pure(Options_Applicative_Types.parserApplicative))(Control_Monad_Trans_Class.lift(Options_Applicative_Internal.nondetTMonadTrans)(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(Control_Monad_State_Trans.StateT(function (args) {
                                return Control_Apply.applyFirst(((dictMonadP.Monad0()).Bind1()).Apply0())(Control_Apply.applySecond(((dictMonadP.Monad0()).Bind1()).Apply0())(Options_Applicative_Internal.enterContext(dictMonadP)(arg)(v1.value0.value0))(runParser(dictMonadP)((Data_Newtype.un()(Options_Applicative_Types.ParserInfo)(v1.value0.value0)).infoPolicy)(Options_Applicative_Types.CmdStart.value)((Data_Newtype.un()(Options_Applicative_Types.ParserInfo)(v1.value0.value0)).infoParser)(args)))(Options_Applicative_Internal.exitContext(dictMonadP));
                            })));
                        };
                        if (v1.value0 instanceof Data_Maybe.Just && v1.value1 instanceof Options_Applicative_Types.SubparserInline) {
                            return Control_Monad_Trans_Class.lift(Options_Applicative_Internal.nondetTMonadTrans)(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(Control_Bind.discard(Control_Bind.discardUnit)(Control_Monad_State_Trans.bindStateT(dictMonadP.Monad0()))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonadP.Monad0())(Options_Applicative_Internal.enterContext(dictMonadP)(arg)(v1.value0.value0)))(function () {
                                return Control_Applicative.pure(Control_Monad_State_Trans.applicativeStateT(dictMonadP.Monad0()))((Data_Newtype.un()(Options_Applicative_Types.ParserInfo)(v1.value0.value0)).infoParser);
                            }));
                        };
                        if (v1.value0 instanceof Data_Maybe.Nothing) {
                            return Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())));
                        };
                        throw new Error("Failed pattern match at Options.Applicative.Common (line 153, column 7 - line 165, column 38): " + [ v1.constructor.name ]);
                    };
                    if (v instanceof Options_Applicative_Types.ArgReader) {
                        return Data_Functor.map(Options_Applicative_Internal.nondetTFunctor(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())))(Control_Applicative.pure(Options_Applicative_Types.parserApplicative))(Control_Monad_Trans_Class.lift(Options_Applicative_Internal.nondetTMonadTrans)(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(Control_Monad_Trans_Class.lift(Control_Monad_State_Trans.monadTransStateT)(dictMonadP.Monad0())(Options_Applicative_Internal.runReadM(dictMonadP)((Data_Newtype.un()(Options_Applicative_Types.CReader)(v.value0)).crReader)(arg))));
                    };
                    return Control_Plus.empty(Options_Applicative_Internal.nondetTPlus(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0())));
                });
            });
        };
    };
};
var runParserInfo = function (dictMonadP) {
    return function (i) {
        return runParserFully(dictMonadP)((Data_Newtype.un()(Options_Applicative_Types.ParserInfo)(i)).infoPolicy)((Data_Newtype.un()(Options_Applicative_Types.ParserInfo)(i)).infoParser);
    };
};
var runParserFully = function (dictMonadP) {
    return function (policy) {
        return function (p) {
            return function (args) {
                return Control_Bind.bind((dictMonadP.Monad0()).Bind1())(runParser(dictMonadP)(policy)(Options_Applicative_Types.CmdStart.value)(p)(args))(function (v) {
                    if (v.value1 instanceof Data_List_Types.Nil) {
                        return Control_Applicative.pure((dictMonadP.Monad0()).Applicative0())(v.value0);
                    };
                    if (v.value1 instanceof Data_List_Types.Cons) {
                        return Options_Applicative_Internal.errorP(dictMonadP)(unexpectedError(v.value1.value0)(Control_Applicative.pure(Options_Applicative_Types.parserApplicative)(Data_Unit.unit)));
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 213, column 3 - line 215, column 66): " + [ v.value1.constructor.name ]);
                });
            };
        };
    };
};
var runParser = function (dictMonadP) {
    return function (policy) {
        return function (isCmdStart) {
            return function (p) {
                return function (args) {
                    var result = Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(evalParser(p)))(Control_Applicative.pure(Data_Maybe.applicativeMaybe)(args));
                    var newPolicy = function (a) {
                        if (policy instanceof Options_Applicative_Types.NoIntersperse) {
                            var $180 = Data_Maybe.isJust(parseWord(a));
                            if ($180) {
                                return Options_Applicative_Types.NoIntersperse.value;
                            };
                            return Options_Applicative_Types.AllPositionals.value;
                        };
                        return policy;
                    };
                    var do_step = function (prefs) {
                        return function (arg) {
                            return function (argt) {
                                return (function (v) {
                                    return Control_Monad_State_Trans.runStateT(v)(argt);
                                })(Options_Applicative_Internal.disamb(Control_Monad_State_Trans.monadStateT(dictMonadP.Monad0()))(!(Data_Newtype.un()(Options_Applicative_Types.ParserPrefs)(prefs)).prefDisambiguate)(stepParser(dictMonadP)(prefs)(policy)(arg)(p)));
                            };
                        };
                    };
                    if (args instanceof Data_List_Types.Nil) {
                        return Options_Applicative_Internal.exitP(dictMonadP)(isCmdStart)(policy)(p)(result);
                    };
                    if (args instanceof Data_List_Types.Cons && (args.value0 === "--" && Data_Eq.notEq(Options_Applicative_Types.argPolicyEq)(policy)(Options_Applicative_Types.AllPositionals.value))) {
                        return runParser(dictMonadP)(Options_Applicative_Types.AllPositionals.value)(Options_Applicative_Types.CmdCont.value)(p)(args.value1);
                    };
                    if (args instanceof Data_List_Types.Cons) {
                        return Control_Bind.bind((dictMonadP.Monad0()).Bind1())(Options_Applicative_Internal.getPrefs(dictMonadP))(function (prefs) {
                            return Control_Bind.bind((dictMonadP.Monad0()).Bind1())(do_step(prefs)(args.value0)(args.value1))(function (v) {
                                if (v.value0 instanceof Data_Maybe.Nothing) {
                                    return Options_Applicative_Internal.hoistMaybe(dictMonadP)(unexpectedError(args.value0)(p))(result);
                                };
                                if (v.value0 instanceof Data_Maybe.Just) {
                                    return runParser(dictMonadP)(newPolicy(args.value0))(Options_Applicative_Types.CmdCont.value)(v.value0.value0)(v.value1);
                                };
                                throw new Error("Failed pattern match at Options.Applicative.Common (line 191, column 5 - line 193, column 60): " + [ v.value0.constructor.name ]);
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Options.Applicative.Common (line 185, column 38 - line 193, column 60): " + [ args.constructor.name ]);
                };
            };
        };
    };
};
var treeMapParser = function (g) {
    var has_default = function (p) {
        return Data_Maybe.isJust(evalParser(p));
    };
    var hasArg = function (v) {
        if (v instanceof Options_Applicative_Types.NilP) {
            return false;
        };
        if (v instanceof Options_Applicative_Types.OptP) {
            return isArg((Data_Newtype.un()(Options_Applicative_Types.Option)(v.value0)).optMain);
        };
        if (v instanceof Options_Applicative_Types.MultP) {
            return Data_Exists.runExists(function (v1) {
                return hasArg(v1.value0) || hasArg(v1.value1);
            })(v.value0);
        };
        if (v instanceof Options_Applicative_Types.AltP) {
            return hasArg(v.value0) || hasArg(v.value1);
        };
        if (v instanceof Options_Applicative_Types.BindP) {
            return Control_Monad_Free["resume'"](function (p) {
                return function (v1) {
                    return hasArg(p);
                };
            })(Data_Function["const"](false))(v.value0);
        };
        throw new Error("Failed pattern match at Options.Applicative.Common (line 271, column 5 - line 271, column 44): " + [ v.constructor.name ]);
    };
    var go = function (v) {
        return function (v1) {
            return function (v2) {
                return function (v3) {
                    return function (v4) {
                        if (v4 instanceof Options_Applicative_Types.NilP) {
                            return new Options_Applicative_Types.MultNode([  ]);
                        };
                        if (v4 instanceof Options_Applicative_Types.OptP) {
                            if (Data_Ord.greaterThan(Options_Applicative_Types.optVisibilityOrd)(Options_Applicative_Types.optVisibility(v4.value0))(Options_Applicative_Types.Internal.value)) {
                                return new Options_Applicative_Types.Leaf(v3({
                                    hinfoMulti: v,
                                    hinfoDefault: v1,
                                    hinfoUnreachableArgs: v2
                                })(v4.value0));
                            };
                            if (Data_Boolean.otherwise) {
                                return new Options_Applicative_Types.MultNode([  ]);
                            };
                        };
                        if (v4 instanceof Options_Applicative_Types.MultP) {
                            return Data_Exists.runExists(function (v5) {
                                var r$prime = v2 || hasArg(v5.value0);
                                return new Options_Applicative_Types.MultNode([ go(v)(v1)(v2)(v3)(v5.value0), go(v)(v1)(r$prime)(v3)(v5.value1) ]);
                            })(v4.value0);
                        };
                        if (v4 instanceof Options_Applicative_Types.AltP) {
                            var d$prime = v1 || (has_default(v4.value0) || has_default(v4.value1));
                            return new Options_Applicative_Types.AltNode([ go(v)(d$prime)(v2)(v3)(v4.value0), go(v)(d$prime)(v2)(v3)(v4.value1) ]);
                        };
                        if (v4 instanceof Options_Applicative_Types.BindP) {
                            return Control_Monad_Free["resume'"](function (p) {
                                return function (k) {
                                    var go$prime = go(true)(v1)(v2)(v3)(p);
                                    var v5 = evalParser(p);
                                    if (v5 instanceof Data_Maybe.Nothing) {
                                        return go$prime;
                                    };
                                    if (v5 instanceof Data_Maybe.Just) {
                                        return new Options_Applicative_Types.MultNode([ go$prime, go(true)(v1)(v2)(v3)(Options_Applicative_Types.BindP.create(k(v5.value0))) ]);
                                    };
                                    throw new Error("Failed pattern match at Options.Applicative.Common (line 266, column 12 - line 268, column 68): " + [ v5.constructor.name ]);
                                };
                            })(Data_Function["const"](new Options_Applicative_Types.MultNode([  ])))(v4.value0);
                        };
                        throw new Error("Failed pattern match at Options.Applicative.Common (line 247, column 5 - line 250, column 21): " + [ v.constructor.name, v1.constructor.name, v2.constructor.name, v3.constructor.name, v4.constructor.name ]);
                    };
                };
            };
        };
    };
    var $233 = go(false)(false)(false)(g);
    return function ($234) {
        return simplify($233($234));
    };
};
var mapParser = function (f) {
    var flatten = function (v) {
        if (v instanceof Options_Applicative_Types.Leaf) {
            return [ v.value0 ];
        };
        if (v instanceof Options_Applicative_Types.MultNode) {
            return Control_Bind.bind(Control_Bind.bindArray)(v.value0)(flatten);
        };
        if (v instanceof Options_Applicative_Types.AltNode) {
            return Control_Bind.bind(Control_Bind.bindArray)(v.value0)(flatten);
        };
        throw new Error("Failed pattern match at Options.Applicative.Common (line 234, column 5 - line 234, column 27): " + [ v.constructor.name ]);
    };
    var $235 = treeMapParser(f);
    return function ($236) {
        return flatten($235($236));
    };
};
module.exports = {
    liftOpt: liftOpt,
    showOption: showOption,
    runParserInfo: runParserInfo,
    runParserFully: runParserFully,
    runParser: runParser,
    evalParser: evalParser,
    mapParser: mapParser,
    treeMapParser: treeMapParser,
    optionNames: optionNames,
    ParserInfo: Options_Applicative_Types.ParserInfo,
    ParserPrefs: Options_Applicative_Types.ParserPrefs
};
