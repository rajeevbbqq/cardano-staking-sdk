// Generated by purs version 0.14.5
"use strict";
var Control_Apply = require("../Control.Apply/index.js");
var Control_Monad_Gen = require("../Control.Monad.Gen/index.js");
var Control_Monad_Gen_Common = require("../Control.Monad.Gen.Common/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_These = require("../Data.These/index.js");
var genThese = function (dictMonadGen) {
    return function (dictMonadRec) {
        return function (ga) {
            return function (gb) {
                return Control_Monad_Gen.filtered(dictMonadRec)(dictMonadGen)(Control_Apply.apply(((dictMonadGen.Monad0()).Bind1()).Apply0())(Data_Functor.map((((dictMonadGen.Monad0()).Bind1()).Apply0()).Functor0())(Data_These.maybeThese)(Control_Monad_Gen_Common.genMaybe(dictMonadGen)(ga)))(Control_Monad_Gen_Common.genMaybe(dictMonadGen)(gb)));
            };
        };
    };
};
module.exports = {
    genThese: genThese
};
