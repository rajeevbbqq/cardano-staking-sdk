// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Functor_App = require("../Data.Functor.App/index.js");
var Data_Functor_Compose = require("../Data.Functor.Compose/index.js");
var Data_Functor_Coproduct = require("../Data.Functor.Coproduct/index.js");
var Data_Functor_Product = require("../Data.Functor.Product/index.js");
var Data_FunctorWithIndex = require("../Data.FunctorWithIndex/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Traversable_Accum = require("../Data.Traversable.Accum/index.js");
var Data_Traversable_Accum_Internal = require("../Data.Traversable.Accum.Internal/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var traverseWithIndexDefault = function (dictTraversableWithIndex) {
    return function (dictApplicative) {
        return function (f) {
            var $64 = Data_Traversable.sequence(dictTraversableWithIndex.Traversable2())(dictApplicative);
            var $65 = Data_FunctorWithIndex.mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0())(f);
            return function ($66) {
                return $64($65($66));
            };
        };
    };
};
var traverseWithIndex = function (dict) {
    return dict.traverseWithIndex;
};
var traverseDefault = function (dictTraversableWithIndex) {
    return function (dictApplicative) {
        return function (f) {
            return traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(Data_Function["const"](f));
        };
    };
};
var traversableWithIndexTuple = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Tuple.Tuple.create(v.value0))(f(Data_Unit.unit)(v.value1));
            };
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexTuple;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexTuple;
    },
    Traversable2: function () {
        return Data_Traversable.traversableTuple;
    }
};
var traversableWithIndexProduct = function (dictTraversableWithIndex) {
    return function (dictTraversableWithIndex1) {
        return {
            traverseWithIndex: function (dictApplicative) {
                return function (f) {
                    return function (v) {
                        return Control_Apply.lift2(dictApplicative.Apply0())(Data_Functor_Product.product)(traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(function ($67) {
                            return f(Data_Either.Left.create($67));
                        })(v.value0))(traverseWithIndex(dictTraversableWithIndex1)(dictApplicative)(function ($68) {
                            return f(Data_Either.Right.create($68));
                        })(v.value1));
                    };
                };
            },
            FunctorWithIndex0: function () {
                return Data_FunctorWithIndex.functorWithIndexProduct(dictTraversableWithIndex.FunctorWithIndex0())(dictTraversableWithIndex1.FunctorWithIndex0());
            },
            FoldableWithIndex1: function () {
                return Data_FoldableWithIndex.foldableWithIndexProduct(dictTraversableWithIndex.FoldableWithIndex1())(dictTraversableWithIndex1.FoldableWithIndex1());
            },
            Traversable2: function () {
                return Data_Traversable.traversableProduct(dictTraversableWithIndex.Traversable2())(dictTraversableWithIndex1.Traversable2());
            }
        };
    };
};
var traversableWithIndexMultiplicative = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableMultiplicative)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexMultiplicative;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexMultiplicative;
    },
    Traversable2: function () {
        return Data_Traversable.traversableMultiplicative;
    }
};
var traversableWithIndexMaybe = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableMaybe)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexMaybe;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexMaybe;
    },
    Traversable2: function () {
        return Data_Traversable.traversableMaybe;
    }
};
var traversableWithIndexLast = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableLast)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexLast;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexLast;
    },
    Traversable2: function () {
        return Data_Traversable.traversableLast;
    }
};
var traversableWithIndexIdentity = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return function (v) {
                return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Identity.Identity)(f(Data_Unit.unit)(v));
            };
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexIdentity;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexIdentity;
    },
    Traversable2: function () {
        return Data_Traversable.traversableIdentity;
    }
};
var traversableWithIndexFirst = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableFirst)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexFirst;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexFirst;
    },
    Traversable2: function () {
        return Data_Traversable.traversableFirst;
    }
};
var traversableWithIndexEither = {
    traverseWithIndex: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                if (v1 instanceof Data_Either.Left) {
                    return Control_Applicative.pure(dictApplicative)(new Data_Either.Left(v1.value0));
                };
                if (v1 instanceof Data_Either.Right) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Either.Right.create)(v(Data_Unit.unit)(v1.value0));
                };
                throw new Error("Failed pattern match at Data.TraversableWithIndex (line 95, column 1 - line 97, column 53): " + [ v.constructor.name, v1.constructor.name ]);
            };
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexEither;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexEither;
    },
    Traversable2: function () {
        return Data_Traversable.traversableEither;
    }
};
var traversableWithIndexDual = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableDual)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexDual;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexDual;
    },
    Traversable2: function () {
        return Data_Traversable.traversableDual;
    }
};
var traversableWithIndexDisj = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableDisj)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexDisj;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexDisj;
    },
    Traversable2: function () {
        return Data_Traversable.traversableDisj;
    }
};
var traversableWithIndexCoproduct = function (dictTraversableWithIndex) {
    return function (dictTraversableWithIndex1) {
        return {
            traverseWithIndex: function (dictApplicative) {
                return function (f) {
                    return Data_Functor_Coproduct.coproduct((function () {
                        var $69 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($72) {
                            return Data_Functor_Coproduct.Coproduct(Data_Either.Left.create($72));
                        });
                        var $70 = traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(function ($73) {
                            return f(Data_Either.Left.create($73));
                        });
                        return function ($71) {
                            return $69($70($71));
                        };
                    })())((function () {
                        var $74 = Data_Functor.map((dictApplicative.Apply0()).Functor0())(function ($77) {
                            return Data_Functor_Coproduct.Coproduct(Data_Either.Right.create($77));
                        });
                        var $75 = traverseWithIndex(dictTraversableWithIndex1)(dictApplicative)(function ($78) {
                            return f(Data_Either.Right.create($78));
                        });
                        return function ($76) {
                            return $74($75($76));
                        };
                    })());
                };
            },
            FunctorWithIndex0: function () {
                return Data_FunctorWithIndex.functorWithIndexCoproduct(dictTraversableWithIndex.FunctorWithIndex0())(dictTraversableWithIndex1.FunctorWithIndex0());
            },
            FoldableWithIndex1: function () {
                return Data_FoldableWithIndex.foldableWithIndexCoproduct(dictTraversableWithIndex.FoldableWithIndex1())(dictTraversableWithIndex1.FoldableWithIndex1());
            },
            Traversable2: function () {
                return Data_Traversable.traversableCoproduct(dictTraversableWithIndex.Traversable2())(dictTraversableWithIndex1.Traversable2());
            }
        };
    };
};
var traversableWithIndexConst = {
    traverseWithIndex: function (dictApplicative) {
        return function (v) {
            return function (v1) {
                return Control_Applicative.pure(dictApplicative)(v1);
            };
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexConst;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexConst;
    },
    Traversable2: function () {
        return Data_Traversable.traversableConst;
    }
};
var traversableWithIndexConj = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableConj)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexConj;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexConj;
    },
    Traversable2: function () {
        return Data_Traversable.traversableConj;
    }
};
var traversableWithIndexCompose = function (dictTraversableWithIndex) {
    return function (dictTraversableWithIndex1) {
        return {
            traverseWithIndex: function (dictApplicative) {
                return function (f) {
                    return function (v) {
                        return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_Compose.Compose)(traverseWithIndex(dictTraversableWithIndex)(dictApplicative)((function () {
                            var $79 = traverseWithIndex(dictTraversableWithIndex1)(dictApplicative);
                            var $80 = Data_Tuple.curry(f);
                            return function ($81) {
                                return $79($80($81));
                            };
                        })())(v));
                    };
                };
            },
            FunctorWithIndex0: function () {
                return Data_FunctorWithIndex.functorWithIndexCompose(dictTraversableWithIndex.FunctorWithIndex0())(dictTraversableWithIndex1.FunctorWithIndex0());
            },
            FoldableWithIndex1: function () {
                return Data_FoldableWithIndex.foldableWithIndexCompose(dictTraversableWithIndex.FoldableWithIndex1())(dictTraversableWithIndex1.FoldableWithIndex1());
            },
            Traversable2: function () {
                return Data_Traversable.traversableCompose(dictTraversableWithIndex.Traversable2())(dictTraversableWithIndex1.Traversable2());
            }
        };
    };
};
var traversableWithIndexArray = {
    traverseWithIndex: function (dictApplicative) {
        return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexArray;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexArray;
    },
    Traversable2: function () {
        return Data_Traversable.traversableArray;
    }
};
var traversableWithIndexApp = function (dictTraversableWithIndex) {
    return {
        traverseWithIndex: function (dictApplicative) {
            return function (f) {
                return function (v) {
                    return Data_Functor.map((dictApplicative.Apply0()).Functor0())(Data_Functor_App.App)(traverseWithIndex(dictTraversableWithIndex)(dictApplicative)(f)(v));
                };
            };
        },
        FunctorWithIndex0: function () {
            return Data_FunctorWithIndex.functorWithIndexApp(dictTraversableWithIndex.FunctorWithIndex0());
        },
        FoldableWithIndex1: function () {
            return Data_FoldableWithIndex.foldableWithIndexApp(dictTraversableWithIndex.FoldableWithIndex1());
        },
        Traversable2: function () {
            return Data_Traversable.traversableApp(dictTraversableWithIndex.Traversable2());
        }
    };
};
var traversableWithIndexAdditive = {
    traverseWithIndex: function (dictApplicative) {
        return function (f) {
            return Data_Traversable.traverse(Data_Traversable.traversableAdditive)(dictApplicative)(f(Data_Unit.unit));
        };
    },
    FunctorWithIndex0: function () {
        return Data_FunctorWithIndex.functorWithIndexAdditive;
    },
    FoldableWithIndex1: function () {
        return Data_FoldableWithIndex.foldableWithIndexAdditive;
    },
    Traversable2: function () {
        return Data_Traversable.traversableAdditive;
    }
};
var mapAccumRWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateR(traverseWithIndex(dictTraversableWithIndex)(Data_Traversable_Accum_Internal.applicativeStateR)(function (i) {
                    return function (a) {
                        return function (s) {
                            return f(i)(s)(a);
                        };
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanrWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumRWithIndex(dictTraversableWithIndex)(function (i) {
                    return function (b) {
                        return function (a) {
                            var b$prime = f(i)(a)(b);
                            return {
                                accum: b$prime,
                                value: b$prime
                            };
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var mapAccumLWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (s0) {
            return function (xs) {
                return Data_Traversable_Accum_Internal.stateL(traverseWithIndex(dictTraversableWithIndex)(Data_Traversable_Accum_Internal.applicativeStateL)(function (i) {
                    return function (a) {
                        return function (s) {
                            return f(i)(s)(a);
                        };
                    };
                })(xs))(s0);
            };
        };
    };
};
var scanlWithIndex = function (dictTraversableWithIndex) {
    return function (f) {
        return function (b0) {
            return function (xs) {
                return (mapAccumLWithIndex(dictTraversableWithIndex)(function (i) {
                    return function (b) {
                        return function (a) {
                            var b$prime = f(i)(b)(a);
                            return {
                                accum: b$prime,
                                value: b$prime
                            };
                        };
                    };
                })(b0)(xs)).value;
            };
        };
    };
};
var forWithIndex = function (dictApplicative) {
    return function (dictTraversableWithIndex) {
        return Data_Function.flip(traverseWithIndex(dictTraversableWithIndex)(dictApplicative));
    };
};
module.exports = {
    traverseWithIndex: traverseWithIndex,
    traverseWithIndexDefault: traverseWithIndexDefault,
    forWithIndex: forWithIndex,
    scanlWithIndex: scanlWithIndex,
    mapAccumLWithIndex: mapAccumLWithIndex,
    scanrWithIndex: scanrWithIndex,
    mapAccumRWithIndex: mapAccumRWithIndex,
    traverseDefault: traverseDefault,
    traversableWithIndexArray: traversableWithIndexArray,
    traversableWithIndexMaybe: traversableWithIndexMaybe,
    traversableWithIndexFirst: traversableWithIndexFirst,
    traversableWithIndexLast: traversableWithIndexLast,
    traversableWithIndexAdditive: traversableWithIndexAdditive,
    traversableWithIndexDual: traversableWithIndexDual,
    traversableWithIndexConj: traversableWithIndexConj,
    traversableWithIndexDisj: traversableWithIndexDisj,
    traversableWithIndexMultiplicative: traversableWithIndexMultiplicative,
    traversableWithIndexEither: traversableWithIndexEither,
    traversableWithIndexTuple: traversableWithIndexTuple,
    traversableWithIndexIdentity: traversableWithIndexIdentity,
    traversableWithIndexConst: traversableWithIndexConst,
    traversableWithIndexProduct: traversableWithIndexProduct,
    traversableWithIndexCoproduct: traversableWithIndexCoproduct,
    traversableWithIndexCompose: traversableWithIndexCompose,
    traversableWithIndexApp: traversableWithIndexApp
};