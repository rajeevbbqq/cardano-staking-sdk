// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad = require("../Control.Monad/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var modify$prime = $foreign.modifyImpl;
var modify = function (f) {
    return modify$prime(function (s) {
        var s$prime = f(s);
        return {
            state: s$prime,
            value: s$prime
        };
    });
};
var functorST = {
    map: $foreign.map_
};
var monadST = {
    Applicative0: function () {
        return applicativeST;
    },
    Bind1: function () {
        return bindST;
    }
};
var bindST = {
    bind: $foreign.bind_,
    Apply0: function () {
        return applyST;
    }
};
var applyST = {
    apply: Control_Monad.ap(monadST),
    Functor0: function () {
        return functorST;
    }
};
var applicativeST = {
    pure: $foreign.pure_,
    Apply0: function () {
        return applyST;
    }
};
var monadRecST = {
    tailRecM: function (f) {
        return function (a) {
            var isLooping = function (v) {
                if (v instanceof Control_Monad_Rec_Class.Loop) {
                    return true;
                };
                return false;
            };
            var fromDone = function (v) {
                if (v instanceof Control_Monad_Rec_Class.Done) {
                    return v.value0;
                };
                throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 69, column 32 - line 69, column 46): " + [ v.constructor.name ]);
            };
            return Control_Bind.bind(bindST)(Control_Bind.bindFlipped(bindST)($foreign["new"])(f(a)))(function (r) {
                return Control_Bind.discard(Control_Bind.discardUnit)(bindST)($foreign["while"](Data_Functor.map(functorST)(isLooping)($foreign.read(r)))(Control_Bind.bind(bindST)($foreign.read(r))(function (v) {
                    if (v instanceof Control_Monad_Rec_Class.Loop) {
                        return Control_Bind.bind(bindST)(f(v.value0))(function (e) {
                            return Data_Functor["void"](functorST)($foreign.write(e)(r));
                        });
                    };
                    if (v instanceof Control_Monad_Rec_Class.Done) {
                        return Control_Applicative.pure(applicativeST)(Data_Unit.unit);
                    };
                    throw new Error("Failed pattern match at Control.Monad.ST.Internal (line 61, column 18 - line 65, column 28): " + [ v.constructor.name ]);
                })))(function () {
                    return Data_Functor.map(functorST)(fromDone)($foreign.read(r));
                });
            });
        };
    },
    Monad0: function () {
        return monadST;
    }
};
module.exports = {
    "modify'": modify$prime,
    modify: modify,
    functorST: functorST,
    applyST: applyST,
    applicativeST: applicativeST,
    bindST: bindST,
    monadST: monadST,
    monadRecST: monadRecST,
    run: $foreign.run,
    "while": $foreign["while"],
    "for": $foreign["for"],
    foreach: $foreign.foreach,
    "new": $foreign["new"],
    read: $foreign.read,
    write: $foreign.write
};
