// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class/index.js");
var Control_Monad_Writer_Trans = require("../Control.Monad.Writer.Trans/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_These = require("../Data.These/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Mote_Description = require("../Mote.Description/index.js");
var Mote_Plan = require("../Mote.Plan/index.js");
var MoteT = function (x) {
    return x;
};
var test = function (dictMonad) {
    return function (label) {
        var $62 = Control_Monad_Writer_Class.tell(Control_Monad_Writer_Trans.monadTellWriterT(Data_Monoid.monoidArray)(dictMonad));
        var $63 = Control_Applicative.pure(Control_Applicative.applicativeArray);
        var $64 = Mote_Description.test(label);
        return function ($65) {
            return MoteT($62($63($64($65))));
        };
    };
};
var skip = function (dictMonad) {
    return function (v) {
        return Control_Monad_Writer_Class.censor(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(dictMonad))(Data_Functor.map(Data_Functor.functorArray)(Mote_Description.setRunMode(Mote_Description.Skip.value)))(v);
    };
};
var planT = function (dictMonad) {
    return function (v) {
        var isThat = function (v1) {
            if (v1 instanceof Data_These.That) {
                return true;
            };
            return false;
        };
        var goSkip = function (a) {
            return Mote_Plan.Plan(Data_Functor.mapFlipped(Data_Functor.functorArray)(a)(function (v1) {
                if (v1 instanceof Mote_Description.Test) {
                    return new Mote_Plan.Skip(v1.value1.label);
                };
                if (v1 instanceof Mote_Description.Group) {
                    return new Mote_Plan.Group({
                        label: v1.value1.label,
                        bracket: Data_Maybe.Nothing.value,
                        value: goSkip(v1.value1.value)
                    });
                };
                throw new Error("Failed pattern match at Mote.Monad (line 167, column 34 - line 171, column 68): " + [ v1.constructor.name ]);
            }));
        };
        var loop = function (ss) {
            var ps = Data_Functor.map(Data_Functor.functorArray)(go)(ss);
            return Data_Array.mapMaybe((function () {
                var $35 = Data_Foldable.any(Data_Foldable.foldableArray)(Data_HeytingAlgebra.heytingAlgebraBoolean)(isThat)(ps);
                if ($35) {
                    return Data_These.theseRight;
                };
                return Data_These.theseLeft;
            })())(ps);
        };
        var go = function (v1) {
            if (v1 instanceof Mote_Description.Test && v1.value0 instanceof Mote_Description.Skip) {
                var a = new Mote_Plan.Skip(v1.value1.label);
                return new Data_These.Both(a, a);
            };
            if (v1 instanceof Mote_Description.Test && v1.value0 instanceof Mote_Description.Normal) {
                return new Data_These.Both(new Mote_Plan.Test(v1.value1), new Mote_Plan.Skip(v1.value1.label));
            };
            if (v1 instanceof Mote_Description.Test && v1.value0 instanceof Mote_Description.Only) {
                return new Data_These.That(new Mote_Plan.Test(v1.value1));
            };
            if (v1 instanceof Mote_Description.Group && v1.value0 instanceof Mote_Description.Skip) {
                var a = new Mote_Plan.Group({
                    label: v1.value1.label,
                    bracket: Data_Maybe.Nothing.value,
                    value: goSkip(v1.value1.value)
                });
                return new Data_These.Both(a, a);
            };
            if (v1 instanceof Mote_Description.Group && v1.value0 instanceof Mote_Description.Normal) {
                return new Data_These.Both(new Mote_Plan.Group({
                    label: v1.value1.label,
                    bracket: v1.value1.bracket,
                    value: loop(v1.value1.value)
                }), new Mote_Plan.Skip(v1.value1.label));
            };
            if (v1 instanceof Mote_Description.Group && v1.value0 instanceof Mote_Description.Only) {
                return new Data_These.That(new Mote_Plan.Group({
                    label: v1.value1.label,
                    bracket: v1.value1.bracket,
                    value: loop(v1.value1.value)
                }));
            };
            throw new Error("Failed pattern match at Mote.Monad (line 149, column 10 - line 164, column 67): " + [ v1.constructor.name ]);
        };
        return Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function ($66) {
            return loop(Data_Tuple.snd($66));
        })(Control_Monad_Writer_Trans.runWriterT(v));
    };
};
var plan = (function () {
    var $67 = Data_Newtype.un()(Data_Identity.Identity);
    var $68 = planT(Data_Identity.monadIdentity);
    return function ($69) {
        return $67($68($69));
    };
})();
var only = function (dictMonad) {
    return function (v) {
        return Control_Monad_Writer_Class.censor(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(dictMonad))(Data_Functor.map(Data_Functor.functorArray)(Mote_Description.setRunMode(Mote_Description.Only.value)))(v);
    };
};
var newtypeMoteT = {
    Coercible0: function () {
        return undefined;
    }
};
var monadTransMoteT = Control_Monad_Writer_Trans.monadTransWriterT(Data_Monoid.monoidArray);
var monadReaderMoteT = function (dictMonadReader) {
    return Control_Monad_Writer_Trans.monadReaderWriterT(Data_Monoid.monoidArray)(dictMonadReader);
};
var monadMoteT = function (dictMonad) {
    return Control_Monad_Writer_Trans.monadWriterT(Data_Monoid.monoidArray)(dictMonad);
};
var monadEffectMoteT = function (dictMonadEffect) {
    return Control_Monad_Writer_Trans.monadEffectWriter(Data_Monoid.monoidArray)(dictMonadEffect);
};
var monadAskMoteT = function (dictMonadAsk) {
    return Control_Monad_Writer_Trans.monadAskWriterT(Data_Monoid.monoidArray)(dictMonadAsk);
};
var mapTest = function (dictFunctor) {
    return function (f) {
        return Data_Newtype.over()()(MoteT)(Control_Monad_Writer_Trans.mapWriterT(Data_Functor.map(dictFunctor)(Data_Functor.map(Data_Tuple.functorTuple)(Data_Functor.map(Data_Functor.functorArray)(Data_Functor.map(Mote_Description.functorDescription)(f))))));
    };
};
var hoist = function (nat) {
    return Data_Newtype.over()()(MoteT)(Control_Monad_Writer_Trans.mapWriterT(nat));
};
var group = function (dictMonad) {
    return function (label) {
        return function (v) {
            return Control_Monad_Writer_Class.censor(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(dictMonad))((function () {
                var $70 = Control_Applicative.pure(Control_Applicative.applicativeArray);
                var $71 = Mote_Description.group(label);
                return function ($72) {
                    return $70($71($72));
                };
            })())(v);
        };
    };
};
var functorMoteT = function (dictFunctor) {
    return Control_Monad_Writer_Trans.functorWriterT(dictFunctor);
};
var bracket = function (dictMonad) {
    return function (b) {
        return function (v) {
            return Control_Monad_Writer_Class.censor(Control_Monad_Writer_Trans.monadWriterWriterT(Data_Monoid.monoidArray)(dictMonad))(Data_Functor.map(Data_Functor.functorArray)(Mote_Description.setBracket(b)))(v);
        };
    };
};
var bindMoteT = function (dictBind) {
    return Control_Monad_Writer_Trans.bindWriterT(Data_Semigroup.semigroupArray)(dictBind);
};
var applyMoteT = function (dictApply) {
    return Control_Monad_Writer_Trans.applyWriterT(Data_Semigroup.semigroupArray)(dictApply);
};
var applicativeMoteT = function (dictApplicative) {
    return Control_Monad_Writer_Trans.applicativeWriterT(Data_Monoid.monoidArray)(dictApplicative);
};
module.exports = {
    MoteT: MoteT,
    hoist: hoist,
    mapTest: mapTest,
    group: group,
    test: test,
    skip: skip,
    only: only,
    bracket: bracket,
    plan: plan,
    planT: planT,
    newtypeMoteT: newtypeMoteT,
    functorMoteT: functorMoteT,
    applyMoteT: applyMoteT,
    applicativeMoteT: applicativeMoteT,
    bindMoteT: bindMoteT,
    monadMoteT: monadMoteT,
    monadTransMoteT: monadTransMoteT,
    monadAskMoteT: monadAskMoteT,
    monadReaderMoteT: monadReaderMoteT,
    monadEffectMoteT: monadEffectMoteT
};
