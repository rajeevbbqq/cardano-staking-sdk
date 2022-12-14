// Generated by purs version 0.14.5
"use strict";
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Category = require("../Control.Category/index.js");
var Control_Comonad = require("../Control.Comonad/index.js");
var Control_Comonad_Env_Class = require("../Control.Comonad.Env.Class/index.js");
var Control_Extend = require("../Control.Extend/index.js");
var Control_Plus = require("../Control.Plus/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var CofreeT = function (x) {
    return x;
};
var runCofreeT = function (v) {
    return v(Data_Unit.unit);
};
var tail = function (dictFunctor) {
    var $103 = Data_Functor.map(dictFunctor)(Data_Tuple.snd);
    return function ($104) {
        return $103(runCofreeT($104));
    };
};
var head = function (dictFunctor) {
    var $105 = Data_Functor.map(dictFunctor)(Data_Tuple.fst);
    return function ($106) {
        return $105(runCofreeT($106));
    };
};
var functorCofreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return {
            map: function (f) {
                return function (v) {
                    return CofreeT(Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(dictFunctor)(Data_Bifunctor.bimap(Data_Bifunctor.bifunctorTuple)(f)(Data_Functor.map(dictFunctor1)(Data_Functor.map(functorCofreeT(dictFunctor)(dictFunctor1))(f)))))(v));
                };
            }
        };
    };
};
var foldableCofreeT = function (dictFoldable) {
    return function (dictFoldable1) {
        return {
            foldMap: function (dictMonoid) {
                return function (f) {
                    return function (v) {
                        var go = function (v1) {
                            return Data_Semigroup.append(dictMonoid.Semigroup0())(f(v1.value0))(Data_Foldable.foldMap(dictFoldable1)(dictMonoid)(Data_Foldable.foldMap(foldableCofreeT(dictFoldable)(dictFoldable1))(dictMonoid)(f))(v1.value1));
                        };
                        return Data_Foldable.foldMap(dictFoldable)(dictMonoid)(go)(v(Data_Unit.unit));
                    };
                };
            },
            foldr: function (abb) {
                return function (b) {
                    return Data_Foldable.foldrDefault(foldableCofreeT(dictFoldable)(dictFoldable1))(abb)(b);
                };
            },
            foldl: function (bab) {
                return function (b) {
                    return Data_Foldable.foldlDefault(foldableCofreeT(dictFoldable)(dictFoldable1))(bab)(b);
                };
            }
        };
    };
};
var comonadTransCofreeT = {
    lower: function (dictComonad) {
        return head((dictComonad.Extend0()).Functor0());
    }
};
var cofreeT$prime = function (t) {
    return CofreeT(function (v) {
        return t;
    });
};
var extendCofreeT = function (dictComonad) {
    return function (dictFunctor) {
        return {
            extend: function (f) {
                return function (v) {
                    var go = function (w) {
                        return Data_Tuple.Tuple.create(f(cofreeT$prime(w)))(Data_Functor.map(dictFunctor)(Control_Extend.extend(extendCofreeT(dictComonad)(dictFunctor))(f))(Data_Tuple.snd(Control_Comonad.extract(dictComonad)(w))));
                    };
                    return CofreeT(function (v1) {
                        return Control_Extend.extend(dictComonad.Extend0())(go)(v(Data_Unit.unit));
                    });
                };
            },
            Functor0: function () {
                return functorCofreeT((dictComonad.Extend0()).Functor0())(dictFunctor);
            }
        };
    };
};
var comonadCofreeT = function (dictComonad) {
    return function (dictFunctor) {
        return {
            extract: (function () {
                var $107 = Control_Comonad.extract(dictComonad);
                var $108 = head((dictComonad.Extend0()).Functor0());
                return function ($109) {
                    return $107($108($109));
                };
            })(),
            Extend0: function () {
                return extendCofreeT(dictComonad)(dictFunctor);
            }
        };
    };
};
var comonadAskCofreeT = function (dictFunctor) {
    return function (dictComonadAsk) {
        return {
            ask: (function () {
                var $110 = Control_Comonad_Env_Class.ask(dictComonadAsk);
                var $111 = tail(((dictComonadAsk.Comonad0()).Extend0()).Functor0());
                return function ($112) {
                    return $110($111($112));
                };
            })(),
            Comonad0: function () {
                return comonadCofreeT(dictComonadAsk.Comonad0())(dictFunctor);
            }
        };
    };
};
var comonadCofreeCofreeT = function (dictComonad) {
    return function (dictFunctor) {
        return {
            unwrapCofree: (function () {
                var $113 = Control_Comonad.extract(dictComonad);
                var $114 = tail((dictComonad.Extend0()).Functor0());
                return function ($115) {
                    return $113($114($115));
                };
            })(),
            Functor0: function () {
                return dictFunctor;
            },
            Comonad1: function () {
                return comonadCofreeT(dictComonad)(dictFunctor);
            }
        };
    };
};
var monadTransCofreeT = function (dictPlus) {
    return {
        lift: function (dictMonad) {
            var go = function (x) {
                return new Data_Tuple.Tuple(x, Control_Plus.empty(dictPlus));
            };
            var $116 = Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(go);
            return function ($117) {
                return cofreeT$prime($116($117));
            };
        }
    };
};
var traversableCofreeT = function (dictTraversable) {
    return function (dictTraversable1) {
        return {
            traverse: function (dictApplicative) {
                return function (f) {
                    return function (v) {
                        var go = function (v1) {
                            return Control_Apply.apply(dictApplicative.Apply0())(Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create)(f(v1.value0)))(Data_Traversable.traverse(dictTraversable1)(dictApplicative)(Data_Traversable.traverse(traversableCofreeT(dictTraversable)(dictTraversable1))(dictApplicative)(f))(v1.value1));
                        };
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(cofreeT$prime)(Data_Traversable.traverse(dictTraversable)(dictApplicative)(go)(v(Data_Unit.unit)));
                    };
                };
            },
            sequence: function (dictApplicative) {
                return Data_Traversable.sequenceDefault(traversableCofreeT(dictTraversable)(dictTraversable1))(dictApplicative);
            },
            Functor0: function () {
                return functorCofreeT(dictTraversable.Functor0())(dictTraversable1.Functor0());
            },
            Foldable1: function () {
                return foldableCofreeT(dictTraversable.Foldable1())(dictTraversable1.Foldable1());
            }
        };
    };
};
var cofreeT = CofreeT;
var bimapCofreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nf) {
            return function (nm) {
                return function (v) {
                    var go = (function () {
                        var $118 = Data_Functor.map(dictFunctor1)(bimapCofreeT(dictFunctor)(dictFunctor1)(nf)(nm));
                        return function ($119) {
                            return $118(nf($119));
                        };
                    })();
                    return CofreeT(Data_Functor.map(Data_Functor.functorFn)(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Tuple.functorTuple)(go)))(Data_Functor.map(Data_Functor.functorFn)(nm)(v)));
                };
            };
        };
    };
};
var hoistCofreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nm) {
            return bimapCofreeT(dictFunctor1)(dictFunctor)(Control_Category.identity(Control_Category.categoryFn))(nm);
        };
    };
};
var interpretCofreeT = function (dictFunctor) {
    return function (dictFunctor1) {
        return function (nf) {
            return bimapCofreeT(dictFunctor1)(dictFunctor)(nf)(Control_Category.identity(Control_Category.categoryFn));
        };
    };
};
var applyCofreeT = function (dictApply) {
    return function (dictApply1) {
        return {
            apply: function (v) {
                return function (v1) {
                    var go = function (v2) {
                        return function (v3) {
                            return new Data_Tuple.Tuple(v2.value0(v3.value0), Control_Apply.lift2(dictApply1)(Control_Apply.apply(applyCofreeT(dictApply)(dictApply1)))(v2.value1)(v3.value1));
                        };
                    };
                    return CofreeT(function (v2) {
                        return Control_Apply.apply(dictApply)(Data_Functor.map(dictApply.Functor0())(go)(v(Data_Unit.unit)))(v1(Data_Unit.unit));
                    });
                };
            },
            Functor0: function () {
                return functorCofreeT(dictApply.Functor0())(dictApply1.Functor0());
            }
        };
    };
};
var bindCofreeT = function (dictMonad) {
    return function (dictAlt) {
        return function (dictApply) {
            return {
                bind: function (v) {
                    return function (f) {
                        return CofreeT(function (v1) {
                            return Control_Bind.bind(dictMonad.Bind1())(v(Data_Unit.unit))(function (v2) {
                                var v3 = f(v2.value0);
                                return Control_Bind.bind(dictMonad.Bind1())(v3(Data_Unit.unit))(function (v4) {
                                    return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Tuple.Tuple(v4.value0, Control_Alt.alt(dictAlt)(v4.value1)(Data_Functor.map(dictAlt.Functor0())(function (v5) {
                                        return Control_Bind.bind(bindCofreeT(dictMonad)(dictAlt)(dictApply))(v5)(f);
                                    })(v2.value1))));
                                });
                            });
                        });
                    };
                },
                Apply0: function () {
                    return applyCofreeT((dictMonad.Bind1()).Apply0())(dictApply);
                }
            };
        };
    };
};
var applicativeCofreeT = function (dictApplicative) {
    return function (dictApply) {
        return function (dictPlus) {
            return {
                pure: function (a) {
                    return CofreeT(function (v) {
                        return Control_Applicative.pure(dictApplicative)(new Data_Tuple.Tuple(a, Control_Plus.empty(dictPlus)));
                    });
                },
                Apply0: function () {
                    return applyCofreeT(dictApplicative.Apply0())(dictApply);
                }
            };
        };
    };
};
var monadCofreeT = function (dictMonad) {
    return function (dictPlus) {
        return function (dictApply) {
            return {
                Applicative0: function () {
                    return applicativeCofreeT(dictMonad.Applicative0())(dictApply)(dictPlus);
                },
                Bind1: function () {
                    return bindCofreeT(dictMonad)(dictPlus.Alt0())(dictApply);
                }
            };
        };
    };
};
var monadEffectCofreeT = function (dictMonadEffect) {
    return function (dictPlus) {
        return function (dictApply) {
            return {
                liftEffect: function (eff) {
                    var go = function (a) {
                        return new Data_Tuple.Tuple(a, Control_Plus.empty(dictPlus));
                    };
                    return cofreeT$prime(Data_Functor.map((((dictMonadEffect.Monad0()).Bind1()).Apply0()).Functor0())(go)(Effect_Class.liftEffect(dictMonadEffect)(eff)));
                },
                Monad0: function () {
                    return monadCofreeT(dictMonadEffect.Monad0())(dictPlus)(dictApply);
                }
            };
        };
    };
};
var monadAffCofreeT = function (dictMonadAff) {
    return function (dictPlus) {
        return function (dictApply) {
            return {
                liftAff: function (aff) {
                    var go = function (a) {
                        return new Data_Tuple.Tuple(a, Control_Plus.empty(dictPlus));
                    };
                    return cofreeT$prime(Data_Functor.map(((((dictMonadAff.MonadEffect0()).Monad0()).Bind1()).Apply0()).Functor0())(go)(Effect_Aff_Class.liftAff(dictMonadAff)(aff)));
                },
                MonadEffect0: function () {
                    return monadEffectCofreeT(dictMonadAff.MonadEffect0())(dictPlus)(dictApply);
                }
            };
        };
    };
};
module.exports = {
    CofreeT: CofreeT,
    cofreeT: cofreeT,
    "cofreeT'": cofreeT$prime,
    runCofreeT: runCofreeT,
    head: head,
    tail: tail,
    hoistCofreeT: hoistCofreeT,
    interpretCofreeT: interpretCofreeT,
    bimapCofreeT: bimapCofreeT,
    functorCofreeT: functorCofreeT,
    applyCofreeT: applyCofreeT,
    applicativeCofreeT: applicativeCofreeT,
    bindCofreeT: bindCofreeT,
    monadCofreeT: monadCofreeT,
    monadTransCofreeT: monadTransCofreeT,
    monadEffectCofreeT: monadEffectCofreeT,
    monadAffCofreeT: monadAffCofreeT,
    comonadCofreeCofreeT: comonadCofreeCofreeT,
    comonadTransCofreeT: comonadTransCofreeT,
    comonadAskCofreeT: comonadAskCofreeT,
    foldableCofreeT: foldableCofreeT,
    traversableCofreeT: traversableCofreeT,
    extendCofreeT: extendCofreeT,
    comonadCofreeT: comonadCofreeT
};
