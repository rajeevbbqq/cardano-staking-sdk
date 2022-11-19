// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Pipes_Core = require("../Pipes.Core/index.js");
var Pipes_Internal = require("../Pipes.Internal/index.js");
var $$yield = function (dictMonad) {
    return Pipes_Core.respond(dictMonad);
};
var replaceAwait = function (dictMonad) {
    return function (p1) {
        return function (p2) {
            return Pipes_Core.composeRequest(dictMonad)(Data_Function["const"](p1))(p2);
        };
    };
};
var replaceAwait$prime = function (dictMonad) {
    return Data_Function.flip(replaceAwait(dictMonad));
};
var next = function (dictMonad) {
    var go = function (p) {
        if (p instanceof Pipes_Internal.Request) {
            return Pipes_Internal.closed(p.value0);
        };
        if (p instanceof Pipes_Internal.Respond) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Right(new Data_Tuple.Tuple(p.value0, p.value1(Data_Unit.unit))));
        };
        if (p instanceof Pipes_Internal.M) {
            return Control_Bind.bind(dictMonad.Bind1())(p.value0)(go);
        };
        if (p instanceof Pipes_Internal.Pure) {
            return Control_Applicative.pure(dictMonad.Applicative0())(new Data_Either.Left(p.value0));
        };
        throw new Error("Failed pattern match at Pipes (line 96, column 12 - line 100, column 38): " + [ p.constructor.name ]);
    };
    return go;
};
var $$for = function (dictMonad) {
    return Pipes_Core.composeResponse(dictMonad);
};
var each = function (dictMonad) {
    return function (dictFoldable) {
        return function (xs) {
            return Data_Foldable.foldr(dictFoldable)(function (a) {
                return function (p) {
                    return Control_Apply.applySecond(Pipes_Internal.applyProxy(dictMonad))($$yield(dictMonad)(a))(p);
                };
            })(Control_Applicative.pure(Pipes_Internal.applicativeProxy(dictMonad))(Data_Unit.unit))(xs);
        };
    };
};
var discard = function (dictMonad) {
    return function (v) {
        return Control_Applicative.pure(dictMonad.Applicative0())(Data_Unit.unit);
    };
};
var composePipes = function (dictMonad) {
    return function (p1) {
        return function (p2) {
            return Pipes_Core["composePull'"](dictMonad)(Data_Function["const"](p1))(p2);
        };
    };
};
var composePipes$prime = function (dictMonad) {
    return Data_Function.flip(composePipes(dictMonad));
};
var composeLoopBodies = function (dictMonad) {
    return Pipes_Core["composeResponse'"](dictMonad);
};
var composeLoopBodies$prime = function (dictMonad) {
    return Data_Function.flip(composeLoopBodies(dictMonad));
};
var cat = function (dictMonad) {
    return Pipes_Core.pull(dictMonad)(Data_Unit.unit);
};
var $$await = function (dictMonad) {
    return Pipes_Core.request(dictMonad)(Data_Unit.unit);
};
module.exports = {
    "for": $$for,
    composeLoopBodies: composeLoopBodies,
    "composeLoopBodies'": composeLoopBodies$prime,
    "await": $$await,
    replaceAwait: replaceAwait,
    "replaceAwait'": replaceAwait$prime,
    cat: cat,
    composePipes: composePipes,
    "composePipes'": composePipes$prime,
    "yield": $$yield,
    next: next,
    each: each,
    discard: discard
};
