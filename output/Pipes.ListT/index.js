// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Monad_State_Class = require("../Control.Monad.State.Class/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Pipes = require("../Pipes/index.js");
var Pipes_Core = require("../Pipes.Core/index.js");
var Pipes_Internal = require("../Pipes.Internal/index.js");
var Select = function (x) {
    return x;
};
var toListT = function (dict) {
    return dict.toListT;
};
var maybeTEnumerable = {
    toListT: function (dictMonad) {
        return function (m) {
            return Select(Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(dictMonad)(Control_Monad_Maybe_Trans.runMaybeT(m)))(function (x) {
                if (x instanceof Data_Maybe.Nothing) {
                    return Control_Applicative.pure(Pipes_Internal.applicativeProxy(dictMonad))(Data_Unit.unit);
                };
                if (x instanceof Data_Maybe.Just) {
                    return Pipes["yield"](dictMonad)(x.value0);
                };
                throw new Error("Failed pattern match at Pipes.ListT (line 122, column 9 - line 124, column 31): " + [ x.constructor.name ]);
            }));
        };
    }
};
var listTMonadTrans = {
    lift: function (dictMonad) {
        return function (m) {
            return Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(dictMonad)(m))(Pipes["yield"](dictMonad));
        };
    }
};
var listTFunctor = function (dictMonad) {
    return {
        map: function (f) {
            return function (v) {
                return Pipes["for"](dictMonad)(v)((function () {
                    var $91 = Pipes["yield"](dictMonad);
                    return function ($92) {
                        return $91(f($92));
                    };
                })());
            };
        }
    };
};
var listTEnumerable = {
    toListT: function (dictMonad) {
        return Control_Category.identity(Control_Category.categoryFn);
    }
};
var listTApply = function (dictMonad) {
    return {
        apply: function (v) {
            return function (v1) {
                return Pipes["for"](dictMonad)(v)(function (f) {
                    return Pipes["for"](dictMonad)(v1)(function (x) {
                        return Pipes["yield"](dictMonad)(f(x));
                    });
                });
            };
        },
        Functor0: function () {
            return listTFunctor(dictMonad);
        }
    };
};
var listTApplicative = function (dictMonad) {
    return {
        pure: (function () {
            var $93 = Pipes["yield"](dictMonad);
            return function ($94) {
                return Select($93($94));
            };
        })(),
        Apply0: function () {
            return listTApply(dictMonad);
        }
    };
};
var listTAlt = function (dictMonad) {
    return {
        alt: function (v) {
            return function (v1) {
                return Control_Apply.applySecond(Pipes_Internal.applyProxy(dictMonad))(v)(v1);
            };
        },
        Functor0: function () {
            return listTFunctor(dictMonad);
        }
    };
};
var listTPlus = function (dictMonad) {
    return {
        empty: Control_Applicative.pure(Pipes_Internal.applicativeProxy(dictMonad))(Data_Unit.unit),
        Alt0: function () {
            return listTAlt(dictMonad);
        }
    };
};
var listTAlternative = function (dictMonad) {
    return {
        Applicative0: function () {
            return listTApplicative(dictMonad);
        },
        Plus1: function () {
            return listTPlus(dictMonad);
        }
    };
};
var listTSemigroup = function (dictMonad) {
    return {
        append: Control_Alt.alt(listTAlt(dictMonad))
    };
};
var listTMonoid = function (dictMonad) {
    return {
        mempty: Control_Plus.empty(listTPlus(dictMonad)),
        Semigroup0: function () {
            return listTSemigroup(dictMonad);
        }
    };
};
var errorTEnumerable = {
    toListT: function (dictMonad) {
        return function (m) {
            return Select(Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(dictMonad)(Control_Monad_Except_Trans.runExceptT(m)))(function (x) {
                if (x instanceof Data_Either.Left) {
                    return Control_Applicative.pure(Pipes_Internal.applicativeProxy(dictMonad))(Data_Unit.unit);
                };
                if (x instanceof Data_Either.Right) {
                    return Pipes["yield"](dictMonad)(x.value0);
                };
                throw new Error("Failed pattern match at Pipes.ListT (line 129, column 9 - line 131, column 31): " + [ x.constructor.name ]);
            }));
        };
    }
};
var enumerate = function (v) {
    return v;
};
var every = function (dictMonad) {
    return function (dictEnumerable) {
        return function (it) {
            return Pipes_Core.composeRequest(dictMonad)(Pipes.discard(Pipes_Internal.monadProxy(dictMonad)))(enumerate(toListT(dictEnumerable)(dictMonad)(it)));
        };
    };
};
var listTBind = function (dictMonad) {
    return {
        bind: function (v) {
            return function (f) {
                return Pipes["for"](dictMonad)(v)(function ($95) {
                    return enumerate(f($95));
                });
            };
        },
        Apply0: function () {
            return listTApply(dictMonad);
        }
    };
};
var listTMonad = function (dictMonad) {
    return {
        Applicative0: function () {
            return listTApplicative(dictMonad);
        },
        Bind1: function () {
            return listTBind(dictMonad);
        }
    };
};
var listTMonadAsk = function (dictMonadAsk) {
    return {
        ask: Control_Monad_Trans_Class.lift(listTMonadTrans)(dictMonadAsk.Monad0())(Control_Monad_Reader_Class.ask(dictMonadAsk)),
        Monad0: function () {
            return listTMonad(dictMonadAsk.Monad0());
        }
    };
};
var listTMonadReader = function (dictMonadReader) {
    return {
        local: function (f) {
            return function (v) {
                return Control_Monad_Reader_Class.local(Pipes_Internal.proxyMonadReader(dictMonadReader))(f)(v);
            };
        },
        MonadAsk0: function () {
            return listTMonadAsk(dictMonadReader.MonadAsk0());
        }
    };
};
var listTMonadEffect = function (dictMonadEffect) {
    return {
        liftEffect: (function () {
            var $96 = Control_Monad_Trans_Class.lift(listTMonadTrans)(dictMonadEffect.Monad0());
            var $97 = Effect_Class.liftEffect(dictMonadEffect);
            return function ($98) {
                return $96($97($98));
            };
        })(),
        Monad0: function () {
            return listTMonad(dictMonadEffect.Monad0());
        }
    };
};
var listTMonadState = function (dictMonadState) {
    return {
        state: (function () {
            var $99 = Control_Monad_Trans_Class.lift(listTMonadTrans)(dictMonadState.Monad0());
            var $100 = Control_Monad_State_Class.state(dictMonadState);
            return function ($101) {
                return $99($100($101));
            };
        })(),
        Monad0: function () {
            return listTMonad(dictMonadState.Monad0());
        }
    };
};
var listTMonadTell = function (dictMonoid) {
    return function (dictMonadTell) {
        return {
            tell: (function () {
                var $102 = Control_Monad_Trans_Class.lift(listTMonadTrans)(dictMonadTell.Monad1());
                var $103 = Control_Monad_Writer_Class.tell(dictMonadTell);
                return function ($104) {
                    return $102($103($104));
                };
            })(),
            Semigroup0: dictMonadTell.Semigroup0,
            Monad1: function () {
                return listTMonad(dictMonadTell.Monad1());
            }
        };
    };
};
var listTMonadWriter = function (dictMonoid) {
    return function (dictMonadWriter) {
        return {
            listen: function (v) {
                var go = function (v1) {
                    return function (v2) {
                        if (v1 instanceof Pipes_Internal.Request) {
                            return new Pipes_Internal.Request(v1.value0, function (a) {
                                return go(v1.value1(a))(v2);
                            });
                        };
                        if (v1 instanceof Pipes_Internal.Respond) {
                            return new Pipes_Internal.Respond(new Data_Tuple.Tuple(v1.value0, v2), function (b$prime) {
                                return go(v1.value1(b$prime))(v2);
                            });
                        };
                        if (v1 instanceof Pipes_Internal.M) {
                            return new Pipes_Internal.M(Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(v1.value0))(function (v3) {
                                return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(go(v3.value0)(Data_Semigroup.append((dictMonadWriter.MonadTell1()).Semigroup0())(v2)(v3.value1)));
                            }));
                        };
                        if (v1 instanceof Pipes_Internal.Pure) {
                            return new Pipes_Internal.Pure(v1.value0);
                        };
                        throw new Error("Failed pattern match at Pipes.ListT (line 84, column 9 - line 84, column 62): " + [ v1.constructor.name, v2.constructor.name ]);
                    };
                };
                return go(v)(Data_Monoid.mempty(dictMonadWriter.Monoid0()));
            },
            pass: function (v) {
                var go = function (v1) {
                    return function (v2) {
                        if (v1 instanceof Pipes_Internal.Request) {
                            return new Pipes_Internal.Request(v1.value0, function (a) {
                                return go(v1.value1(a))(v2);
                            });
                        };
                        if (v1 instanceof Pipes_Internal.Respond) {
                            var _2 = function (v3) {
                                return v1.value0.value1(v2);
                            };
                            var _1 = new Pipes_Internal.Respond(v1.value0.value0, function (b$prime) {
                                return go(v1.value1(b$prime))(v1.value0.value1(v2));
                            });
                            return new Pipes_Internal.M(Control_Monad_Writer_Class.pass(dictMonadWriter)(Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(new Data_Tuple.Tuple(_1, _2))));
                        };
                        if (v1 instanceof Pipes_Internal.M) {
                            return new Pipes_Internal.M(Control_Bind.bind(((dictMonadWriter.MonadTell1()).Monad1()).Bind1())(Control_Monad_Writer_Class.listen(dictMonadWriter)(v1.value0))(function (v3) {
                                return Control_Applicative.pure(((dictMonadWriter.MonadTell1()).Monad1()).Applicative0())(go(v3.value0)(Data_Semigroup.append((dictMonadWriter.MonadTell1()).Semigroup0())(v2)(v3.value1)));
                            }));
                        };
                        if (v1 instanceof Pipes_Internal.Pure) {
                            return new Pipes_Internal.Pure(v1.value0);
                        };
                        throw new Error("Failed pattern match at Pipes.ListT (line 93, column 9 - line 93, column 72): " + [ v1.constructor.name, v2.constructor.name ]);
                    };
                };
                return go(v)(Data_Monoid.mempty(dictMonadWriter.Monoid0()));
            },
            Monoid0: dictMonadWriter.Monoid0,
            MonadTell1: function () {
                return listTMonadTell(dictMonadWriter.Monoid0())(dictMonadWriter.MonadTell1());
            }
        };
    };
};
var listTMonadThrow = function (dictMonadThrow) {
    return {
        throwError: (function () {
            var $105 = Control_Monad_Trans_Class.lift(listTMonadTrans)(dictMonadThrow.Monad0());
            var $106 = Control_Monad_Error_Class.throwError(dictMonadThrow);
            return function ($107) {
                return $105($106($107));
            };
        })(),
        Monad0: function () {
            return listTMonad(dictMonadThrow.Monad0());
        }
    };
};
var listTMonadError = function (dictMonadError) {
    return {
        catchError: function (v) {
            return function (f) {
                return Control_Monad_Error_Class.catchError(Pipes_Internal.proxyMonadError(dictMonadError))(v)(function ($108) {
                    return enumerate(f($108));
                });
            };
        },
        MonadThrow0: function () {
            return listTMonadThrow(dictMonadError.MonadThrow0());
        }
    };
};
var runListT = function (dictMonad) {
    return function (l) {
        return Pipes_Core.runEffect(dictMonad)(enumerate(Control_Apply.applySecond(listTApply(dictMonad))(l)(Control_Plus.empty(listTPlus(dictMonad)))));
    };
};
var runListTRec = function (dictMonadRec) {
    return function (l) {
        return Pipes_Core.runEffectRec(dictMonadRec)(enumerate(Control_Apply.applySecond(listTApply(dictMonadRec.Monad0()))(l)(Control_Plus.empty(listTPlus(dictMonadRec.Monad0())))));
    };
};
module.exports = {
    toListT: toListT,
    Select: Select,
    enumerate: enumerate,
    runListT: runListT,
    runListTRec: runListTRec,
    every: every,
    listTFunctor: listTFunctor,
    listTApply: listTApply,
    listTApplicative: listTApplicative,
    listTBind: listTBind,
    listTMonad: listTMonad,
    listTMonadTrans: listTMonadTrans,
    listTAlt: listTAlt,
    listTPlus: listTPlus,
    listTAlternative: listTAlternative,
    listTMonadEffect: listTMonadEffect,
    listTSemigroup: listTSemigroup,
    listTMonoid: listTMonoid,
    listTMonadState: listTMonadState,
    listTMonadTell: listTMonadTell,
    listTMonadWriter: listTMonadWriter,
    listTMonadAsk: listTMonadAsk,
    listTMonadReader: listTMonadReader,
    listTMonadThrow: listTMonadThrow,
    listTMonadError: listTMonadError,
    listTEnumerable: listTEnumerable,
    maybeTEnumerable: maybeTEnumerable,
    errorTEnumerable: errorTEnumerable
};