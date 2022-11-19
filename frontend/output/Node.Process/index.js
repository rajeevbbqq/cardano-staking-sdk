// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Posix_Signal = require("../Data.Posix.Signal/index.js");
var Foreign_Object = require("../Foreign.Object/index.js");
var Node_Platform = require("../Node.Platform/index.js");
var Unsafe_Coerce = require("../Unsafe.Coerce/index.js");
var version = $foreign.process.version;
var stdoutIsTTY = $foreign.process.stdout.isTTY;
var stdout = $foreign.process.stdout;
var stdinIsTTY = $foreign.process.stdin.isTTY;
var stdin = $foreign.process.stdin;
var stderrIsTTY = $foreign.process.stderr.isTTY;
var stderr = $foreign.process.stderr;
var platformStr = $foreign.process.platform;
var platform = Node_Platform.fromString(platformStr);
var pid = $foreign.process.pid;
var onSignal = function (sig) {
    return $foreign.onSignalImpl(Data_Posix_Signal.toString(sig));
};
var mkEffect = Unsafe_Coerce.unsafeCoerce;
var nextTick = function (callback) {
    return mkEffect(function (v) {
        return $foreign.process.nextTick(callback);
    });
};
var lookupMutableObject = function (k) {
    return function (o) {
        return mkEffect(function (v) {
            return Foreign_Object.lookup(k)(o);
        });
    };
};
var lookupEnv = function (k) {
    return lookupMutableObject(k)($foreign.process.env);
};
var getEnv = $foreign.copyObject($foreign.process.env);
var execPath = mkEffect(function (v) {
    return $foreign.process.execPath;
});
var execArgv = $foreign.copyArray($foreign.process.execArgv);
var cwd = $foreign.process.cwd;
var argv = $foreign.copyArray($foreign.process.argv);
module.exports = {
    onSignal: onSignal,
    nextTick: nextTick,
    argv: argv,
    execArgv: execArgv,
    execPath: execPath,
    cwd: cwd,
    getEnv: getEnv,
    lookupEnv: lookupEnv,
    pid: pid,
    platform: platform,
    stdin: stdin,
    stdout: stdout,
    stderr: stderr,
    stdinIsTTY: stdinIsTTY,
    stdoutIsTTY: stdoutIsTTY,
    stderrIsTTY: stderrIsTTY,
    version: version,
    onBeforeExit: $foreign.onBeforeExit,
    onExit: $foreign.onExit,
    onUncaughtException: $foreign.onUncaughtException,
    onUnhandledRejection: $foreign.onUnhandledRejection,
    chdir: $foreign.chdir,
    setEnv: $foreign.setEnv,
    unsetEnv: $foreign.unsetEnv,
    exit: $foreign.exit
};
