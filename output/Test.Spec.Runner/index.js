// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class/index.js");
var Control_Parallel = require("../Control.Parallel/index.js");
var Control_Parallel_Class = require("../Control.Parallel.Class/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Array_NonEmpty = require("../Data.Array.NonEmpty/index.js");
var Data_DateTime_Instant = require("../Data.DateTime.Instant/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Identity = require("../Data.Identity/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Time_Duration = require("../Data.Time.Duration/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_AVar = require("../Effect.Aff.AVar/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Now = require("../Effect.Now/index.js");
var Pipes = require("../Pipes/index.js");
var Pipes_Core = require("../Pipes.Core/index.js");
var Pipes_Internal = require("../Pipes.Internal/index.js");
var Test_Spec = require("../Test.Spec/index.js");
var Test_Spec_Console = require("../Test.Spec.Console/index.js");
var Test_Spec_Result = require("../Test.Spec.Result/index.js");
var Test_Spec_Runner_Event = require("../Test.Spec.Runner.Event/index.js");
var Test_Spec_Speed = require("../Test.Spec.Speed/index.js");
var Test_Spec_Style = require("../Test.Spec.Style/index.js");
var Test_Spec_Summary = require("../Test.Spec.Summary/index.js");
var Test_Spec_Tree = require("../Test.Spec.Tree/index.js");
var mergeProducers = function (dictTraversable) {
    return function (ps) {
        return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Effect_Aff_AVar.empty))(function ($$var) {
            return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Effect_Aff.forkAff((function () {
                var consumer = function (i) {
                    return Control_Apply.applySecond(Pipes_Internal.applyProxy(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Effect_Aff_AVar.put(i)($$var)))(Control_Applicative.pure(Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))(Data_Unit.unit));
                };
                return Control_Bind.bind(Effect_Aff.bindAff)(Control_Parallel.parTraverse(Effect_Aff.parallelAff)(dictTraversable)(function (p) {
                    return Pipes_Core.runEffectRec(Effect_Aff.monadRecAff)(Pipes_Core.composeResponse(Effect_Aff.monadAff)(p)(consumer));
                })(ps))(function (x) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Aff_AVar.kill(Effect_Exception.error("finished"))($$var))(function () {
                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(x);
                    });
                });
            })())))(function (fib) {
                var loop = Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(Effect_Aff_AVar.take($$var))))(function (res) {
                    if (res instanceof Data_Either.Left) {
                        return Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Effect_Aff.joinFiber(fib));
                    };
                    if (res instanceof Data_Either.Right) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(res.value0))(function () {
                            return loop;
                        });
                    };
                    throw new Error("Failed pattern match at Test.Spec.Runner (line 154, column 7 - line 158, column 15): " + [ res.constructor.name ]);
                });
                return loop;
            });
        });
    };
};
var makeTimeout = function (v) {
    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Aff.delay(v))(function () {
        return Effect_Aff.makeAff(function (cb) {
            return Data_Functor.voidRight(Effect.functorEffect)(Data_Monoid.mempty(Effect_Aff.monoidCanceler))(cb(Data_Either.Left.create(Effect_Exception.error("test timed out after " + (Data_Show.show(Data_Show.showInt)(Data_Int.round(v)) + "ms")))));
        });
    });
};
var timeout = function (time) {
    return function (t) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Control_Parallel_Class.sequential(Effect_Aff.parallelAff)(Control_Alt.alt(Effect_Aff.altParAff)(Control_Parallel_Class.parallel(Effect_Aff.parallelAff)(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(makeTimeout(time))))(Control_Parallel_Class.parallel(Effect_Aff.parallelAff)(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(t)))))(Data_Either.either(Control_Monad_Error_Class.throwError(Effect_Aff.monadThrowAff))(Control_Applicative.pure(Effect_Aff.applicativeAff)));
    };
};
var defaultConfig = {
    slow: 75.0,
    timeout: Data_Maybe.Just.create(2000.0),
    exit: true
};
var _run = function (dictFunctor) {
    return function (config) {
        var runGroup = function (v) {
            if (v.test instanceof Test_Spec_Tree.Leaf && v.test.value1 instanceof Data_Maybe.Just) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.Test((function () {
                    if (v.isParallelizable) {
                        return Test_Spec_Runner_Event.Parallel.value;
                    };
                    return Test_Spec_Runner_Event.Sequential.value;
                })(), v.path, v.test.value0)))(function () {
                    var example = v.test.value1.value0.example(function (a) {
                        return a(Data_Unit.unit);
                    });
                    return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Now.now)))(function (start) {
                        return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(Effect_Aff.attempt((function () {
                            if (config.timeout instanceof Data_Maybe.Just) {
                                return timeout(config.timeout.value0)(example);
                            };
                            return example;
                        })())))(function (e) {
                            return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Effect_Class.liftEffect(Pipes_Internal.proxyMonadEffect(Effect_Aff.monadEffectAff))(Effect_Now.now))(function (end) {
                                var duration = Data_Time_Duration.Milliseconds(Data_Function.on(Data_Ring.sub(Data_Ring.ringNumber))((function () {
                                    var $41 = Data_Newtype.un()(Data_Time_Duration.Milliseconds);
                                    return function ($42) {
                                        return $41(Data_DateTime_Instant.unInstant($42));
                                    };
                                })())(end)(start));
                                var res = Data_Either.either(Test_Spec_Result.Failure.create)(Data_Function["const"](new Test_Spec_Result.Success(Test_Spec_Speed.speedOf(config.slow)(duration), duration)))(e);
                                return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.TestEnd(v.path, v.test.value0, res)))(function () {
                                    return Control_Applicative.pure(Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))([ Test_Spec_Tree.Leaf.create(v.test.value0)(new Data_Maybe.Just(res)) ]);
                                });
                            });
                        });
                    });
                });
            };
            if (v.test instanceof Test_Spec_Tree.Leaf && v.test.value1 instanceof Data_Maybe.Nothing) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.Pending(v.path, v.test.value0)))(function () {
                    return Control_Applicative.pure(Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))([ new Test_Spec_Tree.Leaf(v.test.value0, Data_Maybe.Nothing.value) ]);
                });
            };
            if (v.test instanceof Test_Spec_Tree.Node && v.test.value0 instanceof Data_Either.Right) {
                var indexer = function (index) {
                    return function (x) {
                        return {
                            test: x,
                            path: Data_Semigroup.append(Data_Semigroup.semigroupArray)(v.path)([ {
                                name: Data_Maybe.Nothing.value,
                                index: index
                            } ])
                        };
                    };
                };
                return Control_Apply.applyFirst(Pipes_Internal.applyProxy(Effect_Aff.monadAff))(loop(Data_Array.mapWithIndex(indexer)(v.test.value1)))(Control_Monad_Trans_Class.lift(Pipes_Internal.monadTransProxy)(Effect_Aff.monadAff)(v.test.value0.value0(Data_Unit.unit)));
            };
            if (v.test instanceof Test_Spec_Tree.Node && v.test.value0 instanceof Data_Either.Left) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.Suite((function () {
                    if (v.isParallelizable) {
                        return Test_Spec_Runner_Event.Parallel.value;
                    };
                    return Test_Spec_Runner_Event.Sequential.value;
                })(), v.path, v.test.value0.value0)))(function () {
                    var indexer = function (index) {
                        return function (x) {
                            return {
                                test: x,
                                path: Data_Semigroup.append(Data_Semigroup.semigroupArray)(v.path)([ {
                                    name: new Data_Maybe.Just(v.test.value0.value0),
                                    index: index
                                } ])
                            };
                        };
                    };
                    return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(loop(Data_Array.mapWithIndex(indexer)(v.test.value1)))(function (res) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.SuiteEnd(v.path)))(function () {
                            return Control_Applicative.pure(Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))([ new Test_Spec_Tree.Node(new Data_Either.Left(v.test.value0.value0), res) ]);
                        });
                    });
                });
            };
            throw new Error("Failed pattern match at Test.Spec.Runner (line 114, column 47 - line 138, column 38): " + [ v.test.constructor.name ]);
        };
        var loop = function (tests) {
            var noteWithIsAllParallelizable = Data_Functor.map(Data_Functor.functorArray)(function (v) {
                return {
                    isParallelizable: Test_Spec_Tree.isAllParallelizable(v.test),
                    test: v.test,
                    path: v.path
                };
            });
            var groupByIsParallelizable = Data_Array.groupBy(function (a) {
                return function (b) {
                    return a.isParallelizable && b.isParallelizable;
                };
            });
            return Data_Functor.map(Pipes_Internal.functorProxy(Effect_Aff.monadAff))(Control_Bind.join(Control_Bind.bindArray))(Data_Traversable["for"](Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))(Data_Traversable.traversableArray)(groupByIsParallelizable(noteWithIsAllParallelizable(tests)))(function (g) {
                return Data_Functor.map(Pipes_Internal.functorProxy(Effect_Aff.monadAff))(Control_Bind.join(Control_Bind.bindArray))((function () {
                    var $35 = (Data_Array_NonEmpty.head(g)).isParallelizable;
                    if ($35) {
                        return mergeProducers(Data_Traversable.traversableArray)(Data_Functor.map(Data_Functor.functorArray)(runGroup)(Data_Array_NonEmpty.toArray(g)));
                    };
                    return Data_Traversable["for"](Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))(Data_Traversable.traversableArray)(Data_Array_NonEmpty.toArray(g))(runGroup);
                })());
            }));
        };
        var $43 = Data_Functor.map(dictFunctor)(function (tests) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.Start(Test_Spec_Tree.countTests(tests))))(function () {
                var indexer = function (index) {
                    return function (test) {
                        return {
                            test: test,
                            path: [ {
                                name: Data_Maybe.Nothing.value,
                                index: index
                            } ]
                        };
                    };
                };
                return Control_Bind.bind(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(loop(Data_Array.mapWithIndex(indexer)(tests)))(function (r) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Pipes_Internal.bindProxy(Effect_Aff.monadAff))(Pipes["yield"](Effect_Aff.monadAff)(new Test_Spec_Runner_Event.End(r)))(function () {
                        return Control_Applicative.pure(Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))(r);
                    });
                });
            });
        });
        var $44 = Test_Spec.collect(dictFunctor);
        return function ($45) {
            return $43($44($45));
        };
    };
};
var runSpecT = function (dictFunctor) {
    return function (config) {
        return function (reporters) {
            return function (spec) {
                return Data_Functor.mapFlipped(dictFunctor)(_run(dictFunctor)(config)(spec))(function (runner) {
                    var events = Data_Foldable.foldl(Data_Foldable.foldableArray)(Pipes.composePipes(Effect_Aff.monadAff))(runner)(reporters);
                    var drain = Data_Function["const"](Control_Applicative.pure(Pipes_Internal.applicativeProxy(Effect_Aff.monadAff))(Data_Unit.unit));
                    var reportedEvents = Pipes_Core.runEffectRec(Effect_Aff.monadRecAff)(Pipes_Core.composeResponse(Effect_Aff.monadAff)(events)(drain));
                    if (config.exit) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff)(reportedEvents))(function (v) {
                            if (v instanceof Data_Either.Left) {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Test_Spec_Console.write(Test_Spec_Style.styled(Test_Spec_Style.red)(Data_Show.show(Effect_Exception.showError)(v.value0) + "\x0a"))))(function () {
                                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)($foreign.exit(1)))(function () {
                                        return Control_Monad_Error_Class.throwError(Effect_Aff.monadThrowAff)(v.value0);
                                    });
                                });
                            };
                            if (v instanceof Data_Either.Right) {
                                return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)((function () {
                                    var code = (function () {
                                        var $39 = Test_Spec_Summary.successful(v.value0);
                                        if ($39) {
                                            return 0;
                                        };
                                        return 1;
                                    })();
                                    return function __do() {
                                        $foreign.exit(code)();
                                        return v.value0;
                                    };
                                })());
                            };
                            throw new Error("Failed pattern match at Test.Spec.Runner (line 182, column 33 - line 190, column 21): " + [ v.constructor.name ]);
                        });
                    };
                    return reportedEvents;
                });
            };
        };
    };
};
var runSpec$prime = function (config) {
    return function (reporters) {
        return function (spec) {
            return Data_Functor["void"](Effect_Aff.functorAff)(Data_Newtype.un()(Data_Identity.Identity)(runSpecT(Data_Identity.functorIdentity)(config)(reporters)(spec)));
        };
    };
};
var run = function (dictWarn) {
    return runSpec$prime(defaultConfig);
};
var runSpec = runSpec$prime(defaultConfig);
module.exports = {
    run: run,
    runSpecT: runSpecT,
    runSpec: runSpec,
    "runSpec'": runSpec$prime,
    defaultConfig: defaultConfig
};
