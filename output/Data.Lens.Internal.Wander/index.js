// Generated by purs version 0.14.5
"use strict";
var Data_Identity = require("../Data.Identity/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Profunctor_Choice = require("../Data.Profunctor.Choice/index.js");
var Data_Profunctor_Star = require("../Data.Profunctor.Star/index.js");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong/index.js");
var wanderStar = function (dictApplicative) {
    return {
        wander: function (t) {
            return function (v) {
                return t(dictApplicative)(v);
            };
        },
        Strong0: function () {
            return Data_Profunctor_Star.strongStar((dictApplicative.Apply0()).Functor0());
        },
        Choice1: function () {
            return Data_Profunctor_Star.choiceStar(dictApplicative);
        }
    };
};
var wanderFunction = {
    wander: function (t) {
        return Data_Newtype.alaF()()()()(Data_Identity.Identity)(t(Data_Identity.applicativeIdentity));
    },
    Strong0: function () {
        return Data_Profunctor_Strong.strongFn;
    },
    Choice1: function () {
        return Data_Profunctor_Choice.choiceFn;
    }
};
var wander = function (dict) {
    return dict.wander;
};
module.exports = {
    wander: wander,
    wanderFunction: wanderFunction,
    wanderStar: wanderStar
};
