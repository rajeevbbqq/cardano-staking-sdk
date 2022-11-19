// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Comonad_Cofree = require("../Control.Comonad.Cofree/index.js");
var Control_Comonad_Env_Trans = require("../Control.Comonad.Env.Trans/index.js");
var Control_Comonad_Store_Trans = require("../Control.Comonad.Store.Trans/index.js");
var Control_Comonad_Traced_Trans = require("../Control.Comonad.Traced.Trans/index.js");
var Control_Monad_Except_Trans = require("../Control.Monad.Except.Trans/index.js");
var Control_Monad_Free = require("../Control.Monad.Free/index.js");
var Control_Monad_Maybe_Trans = require("../Control.Monad.Maybe.Trans/index.js");
var Control_Monad_RWS_Trans = require("../Control.Monad.RWS.Trans/index.js");
var Control_Monad_Reader_Trans = require("../Control.Monad.Reader.Trans/index.js");
var Control_Monad_State_Trans = require("../Control.Monad.State.Trans/index.js");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Coyoneda = require("../Data.Coyoneda/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Yoneda = require("../Data.Yoneda/index.js");
var mfunctorYoneda = {
    hoist: function (dictMonad) {
        return Data_Yoneda.hoistYoneda;
    }
};
var mmonadYoneda = {
    embed: function (dictMonad) {
        return function (f) {
            return Data_Functor.map(Data_Functor.functorFn)(f)(Data_Yoneda.lowerYoneda);
        };
    },
    MFunctor0: function () {
        return mfunctorYoneda;
    },
    MonadTrans1: function () {
        return Data_Yoneda.monadTransYoneda;
    }
};
var mfunctorWriterT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return nat(Control_Monad_Writer_Trans.runWriterT(m));
            };
        };
    }
};
var mmonadWriterT = function (dictMonoid) {
    return {
        embed: function (dictMonad) {
            return function (f) {
                return function (m) {
                    return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_Writer_Trans.runWriterT(f(Control_Monad_Writer_Trans.runWriterT(m))))(function (v) {
                        return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v.value0.value0, Data_Semigroup.append(dictMonoid.Semigroup0())(v.value0.value1)(v.value1)));
                    });
                };
            };
        },
        MFunctor0: function () {
            return mfunctorWriterT;
        },
        MonadTrans1: function () {
            return Control_Monad_Writer_Trans.monadTransWriterT(dictMonoid);
        }
    };
};
var mfunctorTracedT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return Data_Newtype.over()()(Control_Comonad_Traced_Trans.TracedT)(nat);
        };
    }
};
var mfunctorStoreT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return Data_Newtype.over()()(Control_Comonad_Store_Trans.StoreT)(Data_Bifunctor.lmap(Data_Bifunctor.bifunctorTuple)(nat));
        };
    }
};
var mfunctorStateT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return function (s) {
                    return nat(Control_Monad_State_Trans.runStateT(m)(s));
                };
            };
        };
    }
};
var mfunctorReaderT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return function (i) {
                    return nat(Control_Monad_Reader_Trans.runReaderT(m)(i));
                };
            };
        };
    }
};
var mmonadReaderT = {
    embed: function (dictMonad) {
        return function (f) {
            return function (m) {
                return function (i) {
                    return Control_Monad_Reader_Trans.runReaderT(f(Control_Monad_Reader_Trans.runReaderT(m)(i)))(i);
                };
            };
        };
    },
    MFunctor0: function () {
        return mfunctorReaderT;
    },
    MonadTrans1: function () {
        return Control_Monad_Reader_Trans.monadTransReaderT;
    }
};
var mfunctorRWS = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return function (r) {
                    return function (s) {
                        return nat(Control_Monad_RWS_Trans.runRWST(m)(r)(s));
                    };
                };
            };
        };
    }
};
var mfunctorProduct = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (v) {
                return new Data_Tuple.Tuple(v.value0, nat(v.value1));
            };
        };
    }
};
var mfunctorMaybe = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return nat(Control_Monad_Maybe_Trans.runMaybeT(m));
            };
        };
    }
};
var mmonadMaybeT = {
    embed: function (dictMonad) {
        return function (f) {
            return function (m) {
                return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_Maybe_Trans.runMaybeT(f(Control_Monad_Maybe_Trans.runMaybeT(m))))(function (x) {
                    return Control_Applicative.pure(dictMonad.Applicative0())((function () {
                        if (x instanceof Data_Maybe.Nothing) {
                            return Data_Maybe.Nothing.value;
                        };
                        if (x instanceof Data_Maybe.Just && x.value0 instanceof Data_Maybe.Nothing) {
                            return Data_Maybe.Nothing.value;
                        };
                        if (x instanceof Data_Maybe.Just && x.value0 instanceof Data_Maybe.Just) {
                            return new Data_Maybe.Just(x.value0.value0);
                        };
                        throw new Error("Failed pattern match at Control.Monad.Morph (line 130, column 10 - line 133, column 30): " + [ x.constructor.name ]);
                    })());
                });
            };
        };
    },
    MFunctor0: function () {
        return mfunctorMaybe;
    },
    MonadTrans1: function () {
        return Control_Monad_Maybe_Trans.monadTransMaybeT;
    }
};
var mfunctorFree = {
    hoist: function (dictMonad) {
        return Control_Monad_Free.hoistFree;
    }
};
var mmonadFree = {
    embed: function (dictMonad) {
        return Control_Monad_Free.foldFree(Control_Monad_Free.freeMonadRec);
    },
    MFunctor0: function () {
        return mfunctorFree;
    },
    MonadTrans1: function () {
        return Control_Monad_Free.freeMonadTrans;
    }
};
var mfunctorExceptT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return function (m) {
                return nat(Control_Monad_Except_Trans.runExceptT(m));
            };
        };
    }
};
var mmonadExceptT = {
    embed: function (dictMonad) {
        return function (f) {
            return function (m) {
                return Control_Bind.bind(dictMonad.Bind1())(Control_Monad_Except_Trans.runExceptT(f(Control_Monad_Except_Trans.runExceptT(m))))(function (x) {
                    return Control_Applicative.pure(dictMonad.Applicative0())((function () {
                        if (x instanceof Data_Either.Left) {
                            return new Data_Either.Left(x.value0);
                        };
                        if (x instanceof Data_Either.Right && x.value0 instanceof Data_Either.Left) {
                            return new Data_Either.Left(x.value0.value0);
                        };
                        if (x instanceof Data_Either.Right && x.value0 instanceof Data_Either.Right) {
                            return new Data_Either.Right(x.value0.value0);
                        };
                        throw new Error("Failed pattern match at Control.Monad.Morph (line 122, column 10 - line 125, column 33): " + [ x.constructor.name ]);
                    })());
                });
            };
        };
    },
    MFunctor0: function () {
        return mfunctorExceptT;
    },
    MonadTrans1: function () {
        return Control_Monad_Except_Trans.monadTransExceptT;
    }
};
var mfunctorEnvT = {
    hoist: function (dictMonad) {
        return function (nat) {
            return Data_Newtype.over()()(Control_Comonad_Env_Trans.EnvT)(Data_Functor.map(Data_Tuple.functorTuple)(nat));
        };
    }
};
var mfunctorCoyoneda = {
    hoist: function (dictMonad) {
        return Data_Coyoneda.hoistCoyoneda;
    }
};
var mfunctorCompose = function (dictFunctor) {
    return {
        hoist: function (dictMonad) {
            return function (nat) {
                return function (v) {
                    return Data_Functor.map(dictFunctor)(nat)(v);
                };
            };
        }
    };
};
var mfunctorCofree = {
    hoist: function (dictMonad) {
        return Control_Comonad_Cofree.hoistCofree(((dictMonad.Bind1()).Apply0()).Functor0());
    }
};
var hoist = function (dict) {
    return dict.hoist;
};
var generalize = function (dictMonad) {
    var $58 = Control_Applicative.pure(dictMonad.Applicative0());
    var $59 = Data_Newtype.unwrap();
    return function ($60) {
        return $58($59($60));
    };
};
var embed = function (dict) {
    return dict.embed;
};
var flipEmbed = function (dictMMonad) {
    return function (dictMonad) {
        return function (t) {
            return function (f) {
                return embed(dictMMonad)(dictMonad)(f)(t);
            };
        };
    };
};
var squash = function (dictMonad) {
    return function (dictMMonad) {
        return embed(dictMMonad)(dictMonad)(Control_Category.identity(Control_Category.categoryFn));
    };
};
var composeKleisliRight = function (dictMMonad) {
    return function (dictMonad) {
        return function (f) {
            return function (g) {
                return function (m) {
                    return embed(dictMMonad)(dictMonad)(g)(f(m));
                };
            };
        };
    };
};
var composeKleisliLeft = function (dictMMonad) {
    return function (dictMonad) {
        return function (g) {
            return function (f) {
                return function (m) {
                    return embed(dictMMonad)(dictMonad)(g)(f(m));
                };
            };
        };
    };
};
module.exports = {
    embed: embed,
    hoist: hoist,
    generalize: generalize,
    squash: squash,
    composeKleisliRight: composeKleisliRight,
    composeKleisliLeft: composeKleisliLeft,
    flipEmbed: flipEmbed,
    mfunctorExceptT: mfunctorExceptT,
    mfunctorMaybe: mfunctorMaybe,
    mfunctorReaderT: mfunctorReaderT,
    mfunctorWriterT: mfunctorWriterT,
    mfunctorStateT: mfunctorStateT,
    mfunctorRWS: mfunctorRWS,
    mfunctorCompose: mfunctorCompose,
    mfunctorProduct: mfunctorProduct,
    mfunctorYoneda: mfunctorYoneda,
    mfunctorCoyoneda: mfunctorCoyoneda,
    mfunctorFree: mfunctorFree,
    mfunctorCofree: mfunctorCofree,
    mfunctorEnvT: mfunctorEnvT,
    mfunctorTracedT: mfunctorTracedT,
    mfunctorStoreT: mfunctorStoreT,
    mmonadExceptT: mmonadExceptT,
    mmonadMaybeT: mmonadMaybeT,
    mmonadReaderT: mmonadReaderT,
    mmonadWriterT: mmonadWriterT,
    mmonadFree: mmonadFree,
    mmonadYoneda: mmonadYoneda
};