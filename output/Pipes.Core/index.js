// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Rec_Class = require("../Control.Monad.Rec.Class/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Pipes_Internal = require("../Pipes.Internal/index.js");
var runEffectRec = function (dictMonadRec) {
    var go = function (v) {
        if (v instanceof Pipes_Internal.Request) {
            return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(Control_Monad_Rec_Class.Done.create)(Pipes_Internal.closed(v.value0));
        };
        if (v instanceof Pipes_Internal.Respond) {
            return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(Control_Monad_Rec_Class.Done.create)(Pipes_Internal.closed(v.value0));
        };
        if (v instanceof Pipes_Internal.Pure) {
            return Control_Applicative.pure((dictMonadRec.Monad0()).Applicative0())(new Control_Monad_Rec_Class.Done(v.value0));
        };
        if (v instanceof Pipes_Internal.M) {
            return Data_Functor.map((((dictMonadRec.Monad0()).Bind1()).Apply0()).Functor0())(Control_Monad_Rec_Class.Loop.create)(v.value0);
        };
        throw new Error("Failed pattern match at Pipes.Core (line 104, column 3 - line 104, column 39): " + [ v.constructor.name ]);
    };
    return Control_Monad_Rec_Class.tailRecM(dictMonadRec)(go);
};
var runEffect = function (dictMonad) {
    var go = function (p) {
        if (p instanceof Pipes_Internal.Request) {
            return Pipes_Internal.closed(p.value0);
        };
        if (p instanceof Pipes_Internal.Respond) {
            return Pipes_Internal.closed(p.value0);
        };
        if (p instanceof Pipes_Internal.M) {
            return Control_Bind.bind(dictMonad.Bind1())(p.value0)(go);
        };
        if (p instanceof Pipes_Internal.Pure) {
            return Control_Applicative.pure(dictMonad.Applicative0())(p.value0);
        };
        throw new Error("Failed pattern match at Pipes.Core (line 95, column 12 - line 99, column 30): " + [ p.constructor.name ]);
    };
    return go;
};
var respond = function (dictMonad) {
    return function (a) {
        return new Pipes_Internal.Respond(a, Pipes_Internal.Pure.create);
    };
};
var request = function (dictMonad) {
    return function (a$prime) {
        return new Pipes_Internal.Request(a$prime, Pipes_Internal.Pure.create);
    };
};
var reflect = function (dictMonad) {
    var go = function (p) {
        if (p instanceof Pipes_Internal.Request) {
            return new Pipes_Internal.Respond(p.value0, function ($75) {
                return go(p.value1($75));
            });
        };
        if (p instanceof Pipes_Internal.Respond) {
            return new Pipes_Internal.Request(p.value0, function ($76) {
                return go(p.value1($76));
            });
        };
        if (p instanceof Pipes_Internal.M) {
            return new Pipes_Internal.M(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(go)(p.value0));
        };
        if (p instanceof Pipes_Internal.Pure) {
            return new Pipes_Internal.Pure(p.value0);
        };
        throw new Error("Failed pattern match at Pipes.Core (line 234, column 12 - line 238, column 33): " + [ p.constructor.name ]);
    };
    return go;
};
var push = function (dictMonad) {
    var go = function (a) {
        return new Pipes_Internal.Respond(a, function (a$prime) {
            return new Pipes_Internal.Request(a$prime, go);
        });
    };
    return go;
};
var pull = function (dictMonad) {
    var go = function (a$prime) {
        return new Pipes_Internal.Request(a$prime, function (a) {
            return new Pipes_Internal.Respond(a, go);
        });
    };
    return go;
};
var composeResponse = function (dictMonad) {
    return function (p0) {
        return function (fb) {
            var go = function (p) {
                if (p instanceof Pipes_Internal.Request) {
                    return new Pipes_Internal.Request(p.value0, function ($77) {
                        return go(p.value1($77));
                    });
                };
                if (p instanceof Pipes_Internal.Respond) {
                    return Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad))(fb(p.value0))(function ($78) {
                        return go(p.value1($78));
                    });
                };
                if (p instanceof Pipes_Internal.M) {
                    return new Pipes_Internal.M(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(go)(p.value0));
                };
                if (p instanceof Pipes_Internal.Pure) {
                    return new Pipes_Internal.Pure(p.value0);
                };
                throw new Error("Failed pattern match at Pipes.Core (line 137, column 12 - line 141, column 33): " + [ p.constructor.name ]);
            };
            return go(p0);
        };
    };
};
var composeResponse$prime = function (dictMonad) {
    return function (fa) {
        return function (fb) {
            return function (a) {
                return composeResponse(dictMonad)(fa(a))(fb);
            };
        };
    };
};
var flippedComposeResponse$prime = function (dictMonad) {
    return function (p1) {
        return function (p2) {
            return composeResponse$prime(dictMonad)(p2)(p1);
        };
    };
};
var flippedComposeResponse = function (dictMonad) {
    return function (f) {
        return function (p) {
            return composeResponse(dictMonad)(p)(f);
        };
    };
};
var composeRequest = function (dictMonad) {
    return function (fb$prime) {
        return function (p0) {
            var go = function (p) {
                if (p instanceof Pipes_Internal.Request) {
                    return Control_Bind.bind(Pipes_Internal.bindProxy(dictMonad))(fb$prime(p.value0))(function ($79) {
                        return go(p.value1($79));
                    });
                };
                if (p instanceof Pipes_Internal.Respond) {
                    return new Pipes_Internal.Respond(p.value0, function ($80) {
                        return go(p.value1($80));
                    });
                };
                if (p instanceof Pipes_Internal.M) {
                    return new Pipes_Internal.M(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(go)(p.value0));
                };
                if (p instanceof Pipes_Internal.Pure) {
                    return new Pipes_Internal.Pure(p.value0);
                };
                throw new Error("Failed pattern match at Pipes.Core (line 163, column 12 - line 167, column 33): " + [ p.constructor.name ]);
            };
            return go(p0);
        };
    };
};
var composeRequest$prime = function (dictMonad) {
    return function (fb$prime) {
        return function (fc$prime) {
            return function (c$prime) {
                return composeRequest(dictMonad)(fb$prime)(fc$prime(c$prime));
            };
        };
    };
};
var flippedComposeRequest$prime = function (dictMonad) {
    return function (p1) {
        return function (p2) {
            return composeRequest$prime(dictMonad)(p2)(p1);
        };
    };
};
var flippedComposeRequest = function (dictMonad) {
    return function (p) {
        return function (f) {
            return composeRequest(dictMonad)(f)(p);
        };
    };
};
var composePush$prime = function (dictMonad) {
    return function (p) {
        return function (fb) {
            if (p instanceof Pipes_Internal.Request) {
                return new Pipes_Internal.Request(p.value0, function (a) {
                    return composePush$prime(dictMonad)(p.value1(a))(fb);
                });
            };
            if (p instanceof Pipes_Internal.Respond) {
                return composePull$prime(dictMonad)(p.value1)(fb(p.value0));
            };
            if (p instanceof Pipes_Internal.M) {
                return new Pipes_Internal.M(Control_Bind.bind(dictMonad.Bind1())(p.value0)(function (p$prime) {
                    return Control_Applicative.pure(dictMonad.Applicative0())(composePush$prime(dictMonad)(p$prime)(fb));
                }));
            };
            if (p instanceof Pipes_Internal.Pure) {
                return new Pipes_Internal.Pure(p.value0);
            };
            throw new Error("Failed pattern match at Pipes.Core (line 222, column 21 - line 226, column 29): " + [ p.constructor.name ]);
        };
    };
};
var composePull$prime = function (dictMonad) {
    return function (fb$prime) {
        return function (p) {
            if (p instanceof Pipes_Internal.Request) {
                return composePush$prime(dictMonad)(fb$prime(p.value0))(p.value1);
            };
            if (p instanceof Pipes_Internal.Respond) {
                return new Pipes_Internal.Respond(p.value0, function ($81) {
                    return (function (v) {
                        return composePull$prime(dictMonad)(fb$prime)(v);
                    })(p.value1($81));
                });
            };
            if (p instanceof Pipes_Internal.M) {
                return new Pipes_Internal.M(Data_Functor.map(((dictMonad.Bind1()).Apply0()).Functor0())(function (v) {
                    return composePull$prime(dictMonad)(fb$prime)(v);
                })(p.value0));
            };
            if (p instanceof Pipes_Internal.Pure) {
                return new Pipes_Internal.Pure(p.value0);
            };
            throw new Error("Failed pattern match at Pipes.Core (line 197, column 22 - line 201, column 29): " + [ p.constructor.name ]);
        };
    };
};
var composePush = function (dictMonad) {
    return function (fa) {
        return function (fb) {
            return function (a) {
                return composePush$prime(dictMonad)(fa(a))(fb);
            };
        };
    };
};
var flippedComposePush = function (dictMonad) {
    return function (p1) {
        return function (p2) {
            return composePush(dictMonad)(p2)(p1);
        };
    };
};
var flippedComposePush$prime = function (dictMonad) {
    return function (k) {
        return function (p) {
            return composePush$prime(dictMonad)(p)(k);
        };
    };
};
var flippedComposePull$prime = function (dictMonad) {
    return function (k) {
        return function (p) {
            return composePull$prime(dictMonad)(p)(k);
        };
    };
};
var composePull = function (dictMonad) {
    return function (fb$prime) {
        return function (fc$prime) {
            return function (c$prime) {
                return composePull$prime(dictMonad)(fb$prime)(fc$prime(c$prime));
            };
        };
    };
};
var flippedComposePull = function (dictMonad) {
    return function (p1) {
        return function (p2) {
            return composePull(dictMonad)(p2)(p1);
        };
    };
};
module.exports = {
    runEffect: runEffect,
    runEffectRec: runEffectRec,
    respond: respond,
    "composeResponse'": composeResponse$prime,
    composeResponse: composeResponse,
    request: request,
    "composeRequest'": composeRequest$prime,
    composeRequest: composeRequest,
    push: push,
    composePush: composePush,
    "composePush'": composePush$prime,
    pull: pull,
    composePull: composePull,
    "composePull'": composePull$prime,
    reflect: reflect,
    "flippedComposeResponse'": flippedComposeResponse$prime,
    "flippedComposeRequest'": flippedComposeRequest$prime,
    flippedComposePush: flippedComposePush,
    "flippedComposePush'": flippedComposePush$prime,
    flippedComposePull: flippedComposePull,
    "flippedComposePull'": flippedComposePull$prime,
    flippedComposeResponse: flippedComposeResponse,
    flippedComposeRequest: flippedComposeRequest,
    closed: Pipes_Internal.closed
};
