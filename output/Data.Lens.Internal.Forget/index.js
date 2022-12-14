// Generated by purs version 0.14.5
"use strict";
var Data_Const = require("../Data.Const/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Forget = function (x) {
    return x;
};
var semigroupForget = function (dictSemigroup) {
    return Data_Semigroup.semigroupFn(dictSemigroup);
};
var profunctorForget = {
    dimap: function (f) {
        return function (v) {
            return function (v1) {
                return function ($24) {
                    return v1(f($24));
                };
            };
        };
    }
};
var strongForget = {
    first: function (v) {
        return function ($25) {
            return v(Data_Tuple.fst($25));
        };
    },
    second: function (v) {
        return function ($26) {
            return v(Data_Tuple.snd($26));
        };
    },
    Profunctor0: function () {
        return profunctorForget;
    }
};
var newtypeForget = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidForget = function (dictMonoid) {
    return Data_Monoid.monoidFn(dictMonoid);
};
var cochoiceForget = {
    unleft: function (v) {
        return function ($27) {
            return v(Data_Either.Left.create($27));
        };
    },
    unright: function (v) {
        return function ($28) {
            return v(Data_Either.Right.create($28));
        };
    },
    Profunctor0: function () {
        return profunctorForget;
    }
};
var choiceForget = function (dictMonoid) {
    return {
        left: function (v) {
            return Data_Either.either(v)(Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid)));
        },
        right: function (v) {
            return Data_Either.either(Data_Monoid.mempty(Data_Monoid.monoidFn(dictMonoid)))(v);
        },
        Profunctor0: function () {
            return profunctorForget;
        }
    };
};
var wanderForget = function (dictMonoid) {
    return {
        wander: function (f) {
            return function (v) {
                return Data_Newtype.alaF()()()()(Data_Const.Const)(f(Data_Const.applicativeConst(dictMonoid)))(v);
            };
        },
        Strong0: function () {
            return strongForget;
        },
        Choice1: function () {
            return choiceForget(dictMonoid);
        }
    };
};
module.exports = {
    Forget: Forget,
    newtypeForget: newtypeForget,
    semigroupForget: semigroupForget,
    monoidForget: monoidForget,
    profunctorForget: profunctorForget,
    choiceForget: choiceForget,
    strongForget: strongForget,
    cochoiceForget: cochoiceForget,
    wanderForget: wanderForget
};
