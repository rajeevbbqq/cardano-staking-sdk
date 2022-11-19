// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Posix_Signal = require("../Data.Posix.Signal/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var Node_ChildProcess = require("../Node.ChildProcess/index.js");
var Node_Encoding = require("../Node.Encoding/index.js");
var Node_Process = require("../Node.Process/index.js");
var Node_Stream = require("../Node.Stream/index.js");
var NoOp = (function () {
    function NoOp() {

    };
    NoOp.value = new NoOp();
    return NoOp;
})();
var Success = (function () {
    function Success() {

    };
    Success.value = new Success();
    return Success;
})();
var Cancel = (function () {
    function Cancel() {

    };
    Cancel.value = new Cancel();
    return Cancel;
})();
var spawnAndWaitForOutput$prime = function (cmd) {
    return function (args) {
        return function (opts) {
            return function (filter) {
                return function (cont) {
                    return function __do() {
                        var child = Node_ChildProcess.spawn(cmd)(args)(opts)();
                        var ref = Effect_Ref["new"](new Data_Maybe.Just(""))();
                        Node_ChildProcess.onExit(child)(Data_Function["const"](function __do() {
                            var output = Effect_Ref.read(ref)();
                            return cont(Data_Either.Left.create(Effect_Exception.error("Process " + (cmd + (" exited. Output:\x0a" + Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidString)(output))))))();
                        }))();
                        Node_Stream.onDataString(Node_ChildProcess.stdout(child))(Node_Encoding.UTF8.value)(function (str) {
                            return function __do() {
                                var output = Effect_Ref.modify(Data_Functor.map(Data_Maybe.functorMaybe)(function (v) {
                                    return v + str;
                                }))(ref)();
                                var v = Data_Functor.map(Data_Maybe.functorMaybe)(filter)(output);
                                if (v instanceof Data_Maybe.Just && v.value0 instanceof Success) {
                                    Effect_Ref.write(Data_Maybe.Nothing.value)(ref)();
                                    return cont(Control_Applicative.pure(Data_Either.applicativeEither)(child))();
                                };
                                if (v instanceof Data_Maybe.Just && v.value0 instanceof Cancel) {
                                    Effect_Ref.write(Data_Maybe.Nothing.value)(ref)();
                                    Node_ChildProcess.kill(Data_Posix_Signal.SIGINT.value)(child)();
                                    return cont(Data_Either.Left.create(Effect_Exception.error("Process cancelled because output received: " + str)))();
                                };
                                return Data_Unit.unit;
                            };
                        })();
                        return Effect_Aff.Canceler(Data_Function["const"](Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Node_ChildProcess.kill(Data_Posix_Signal.SIGINT.value)(child))));
                    };
                };
            };
        };
    };
};
var spawnAndWaitForOutput = function (cmd) {
    return function (args) {
        return function (opts) {
            return function (filter) {
                return Effect_Aff.makeAff(spawnAndWaitForOutput$prime(cmd)(args)(opts)(filter));
            };
        };
    };
};
var killOnExit = function (child) {
    return function __do() {
        var aliveRef = Effect_Ref["new"](true)();
        Node_ChildProcess.onExit(child)(function (v) {
            return Effect_Ref.write(false)(aliveRef);
        })();
        return Node_Process.onExit(function (v) {
            return function __do() {
                var alive = Effect_Ref.read(aliveRef)();
                return Control_Applicative.when(Effect.applicativeEffect)(alive)(Node_ChildProcess.kill(Data_Posix_Signal.SIGINT.value)(child))();
            };
        })();
    };
};
module.exports = {
    NoOp: NoOp,
    Success: Success,
    Cancel: Cancel,
    spawnAndWaitForOutput: spawnAndWaitForOutput,
    killOnExit: killOnExit
};