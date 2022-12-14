// Generated by purs version 0.14.5
"use strict";
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var SIGABRT = (function () {
    function SIGABRT() {

    };
    SIGABRT.value = new SIGABRT();
    return SIGABRT;
})();
var SIGALRM = (function () {
    function SIGALRM() {

    };
    SIGALRM.value = new SIGALRM();
    return SIGALRM;
})();
var SIGBUS = (function () {
    function SIGBUS() {

    };
    SIGBUS.value = new SIGBUS();
    return SIGBUS;
})();
var SIGCHLD = (function () {
    function SIGCHLD() {

    };
    SIGCHLD.value = new SIGCHLD();
    return SIGCHLD;
})();
var SIGCLD = (function () {
    function SIGCLD() {

    };
    SIGCLD.value = new SIGCLD();
    return SIGCLD;
})();
var SIGCONT = (function () {
    function SIGCONT() {

    };
    SIGCONT.value = new SIGCONT();
    return SIGCONT;
})();
var SIGEMT = (function () {
    function SIGEMT() {

    };
    SIGEMT.value = new SIGEMT();
    return SIGEMT;
})();
var SIGFPE = (function () {
    function SIGFPE() {

    };
    SIGFPE.value = new SIGFPE();
    return SIGFPE;
})();
var SIGHUP = (function () {
    function SIGHUP() {

    };
    SIGHUP.value = new SIGHUP();
    return SIGHUP;
})();
var SIGILL = (function () {
    function SIGILL() {

    };
    SIGILL.value = new SIGILL();
    return SIGILL;
})();
var SIGINFO = (function () {
    function SIGINFO() {

    };
    SIGINFO.value = new SIGINFO();
    return SIGINFO;
})();
var SIGINT = (function () {
    function SIGINT() {

    };
    SIGINT.value = new SIGINT();
    return SIGINT;
})();
var SIGIO = (function () {
    function SIGIO() {

    };
    SIGIO.value = new SIGIO();
    return SIGIO;
})();
var SIGIOT = (function () {
    function SIGIOT() {

    };
    SIGIOT.value = new SIGIOT();
    return SIGIOT;
})();
var SIGKILL = (function () {
    function SIGKILL() {

    };
    SIGKILL.value = new SIGKILL();
    return SIGKILL;
})();
var SIGLOST = (function () {
    function SIGLOST() {

    };
    SIGLOST.value = new SIGLOST();
    return SIGLOST;
})();
var SIGPIPE = (function () {
    function SIGPIPE() {

    };
    SIGPIPE.value = new SIGPIPE();
    return SIGPIPE;
})();
var SIGPOLL = (function () {
    function SIGPOLL() {

    };
    SIGPOLL.value = new SIGPOLL();
    return SIGPOLL;
})();
var SIGPROF = (function () {
    function SIGPROF() {

    };
    SIGPROF.value = new SIGPROF();
    return SIGPROF;
})();
var SIGPWR = (function () {
    function SIGPWR() {

    };
    SIGPWR.value = new SIGPWR();
    return SIGPWR;
})();
var SIGQUIT = (function () {
    function SIGQUIT() {

    };
    SIGQUIT.value = new SIGQUIT();
    return SIGQUIT;
})();
var SIGSEGV = (function () {
    function SIGSEGV() {

    };
    SIGSEGV.value = new SIGSEGV();
    return SIGSEGV;
})();
var SIGSTKFLT = (function () {
    function SIGSTKFLT() {

    };
    SIGSTKFLT.value = new SIGSTKFLT();
    return SIGSTKFLT;
})();
var SIGSTOP = (function () {
    function SIGSTOP() {

    };
    SIGSTOP.value = new SIGSTOP();
    return SIGSTOP;
})();
var SIGSYS = (function () {
    function SIGSYS() {

    };
    SIGSYS.value = new SIGSYS();
    return SIGSYS;
})();
var SIGTERM = (function () {
    function SIGTERM() {

    };
    SIGTERM.value = new SIGTERM();
    return SIGTERM;
})();
var SIGTRAP = (function () {
    function SIGTRAP() {

    };
    SIGTRAP.value = new SIGTRAP();
    return SIGTRAP;
})();
var SIGTSTP = (function () {
    function SIGTSTP() {

    };
    SIGTSTP.value = new SIGTSTP();
    return SIGTSTP;
})();
var SIGTTIN = (function () {
    function SIGTTIN() {

    };
    SIGTTIN.value = new SIGTTIN();
    return SIGTTIN;
})();
var SIGTTOU = (function () {
    function SIGTTOU() {

    };
    SIGTTOU.value = new SIGTTOU();
    return SIGTTOU;
})();
var SIGUNUSED = (function () {
    function SIGUNUSED() {

    };
    SIGUNUSED.value = new SIGUNUSED();
    return SIGUNUSED;
})();
var SIGURG = (function () {
    function SIGURG() {

    };
    SIGURG.value = new SIGURG();
    return SIGURG;
})();
var SIGUSR1 = (function () {
    function SIGUSR1() {

    };
    SIGUSR1.value = new SIGUSR1();
    return SIGUSR1;
})();
var SIGUSR2 = (function () {
    function SIGUSR2() {

    };
    SIGUSR2.value = new SIGUSR2();
    return SIGUSR2;
})();
var SIGVTALRM = (function () {
    function SIGVTALRM() {

    };
    SIGVTALRM.value = new SIGVTALRM();
    return SIGVTALRM;
})();
var SIGWINCH = (function () {
    function SIGWINCH() {

    };
    SIGWINCH.value = new SIGWINCH();
    return SIGWINCH;
})();
var SIGXCPU = (function () {
    function SIGXCPU() {

    };
    SIGXCPU.value = new SIGXCPU();
    return SIGXCPU;
})();
var SIGXFSZ = (function () {
    function SIGXFSZ() {

    };
    SIGXFSZ.value = new SIGXFSZ();
    return SIGXFSZ;
})();
var toString = function (s) {
    if (s instanceof SIGABRT) {
        return "SIGABRT";
    };
    if (s instanceof SIGALRM) {
        return "SIGALRM";
    };
    if (s instanceof SIGBUS) {
        return "SIGBUS";
    };
    if (s instanceof SIGCHLD) {
        return "SIGCHLD";
    };
    if (s instanceof SIGCLD) {
        return "SIGCLD";
    };
    if (s instanceof SIGCONT) {
        return "SIGCONT";
    };
    if (s instanceof SIGEMT) {
        return "SIGEMT";
    };
    if (s instanceof SIGFPE) {
        return "SIGFPE";
    };
    if (s instanceof SIGHUP) {
        return "SIGHUP";
    };
    if (s instanceof SIGILL) {
        return "SIGILL";
    };
    if (s instanceof SIGINFO) {
        return "SIGINFO";
    };
    if (s instanceof SIGINT) {
        return "SIGINT";
    };
    if (s instanceof SIGIO) {
        return "SIGIO";
    };
    if (s instanceof SIGIOT) {
        return "SIGIOT";
    };
    if (s instanceof SIGKILL) {
        return "SIGKILL";
    };
    if (s instanceof SIGLOST) {
        return "SIGLOST";
    };
    if (s instanceof SIGPIPE) {
        return "SIGPIPE";
    };
    if (s instanceof SIGPOLL) {
        return "SIGPOLL";
    };
    if (s instanceof SIGPROF) {
        return "SIGPROF";
    };
    if (s instanceof SIGPWR) {
        return "SIGPWR";
    };
    if (s instanceof SIGQUIT) {
        return "SIGQUIT";
    };
    if (s instanceof SIGSEGV) {
        return "SIGSEGV";
    };
    if (s instanceof SIGSTKFLT) {
        return "SIGSTKFLT";
    };
    if (s instanceof SIGSTOP) {
        return "SIGSTOP";
    };
    if (s instanceof SIGSYS) {
        return "SIGSYS";
    };
    if (s instanceof SIGTERM) {
        return "SIGTERM";
    };
    if (s instanceof SIGTRAP) {
        return "SIGTRAP";
    };
    if (s instanceof SIGTSTP) {
        return "SIGTSTP";
    };
    if (s instanceof SIGTTIN) {
        return "SIGTTIN";
    };
    if (s instanceof SIGTTOU) {
        return "SIGTTOU";
    };
    if (s instanceof SIGUNUSED) {
        return "SIGUNUSED";
    };
    if (s instanceof SIGURG) {
        return "SIGURG";
    };
    if (s instanceof SIGUSR1) {
        return "SIGUSR1";
    };
    if (s instanceof SIGUSR2) {
        return "SIGUSR2";
    };
    if (s instanceof SIGVTALRM) {
        return "SIGVTALRM";
    };
    if (s instanceof SIGWINCH) {
        return "SIGWINCH";
    };
    if (s instanceof SIGXCPU) {
        return "SIGXCPU";
    };
    if (s instanceof SIGXFSZ) {
        return "SIGXFSZ";
    };
    throw new Error("Failed pattern match at Data.Posix.Signal (line 48, column 14 - line 86, column 24): " + [ s.constructor.name ]);
};
var showSignal = {
    show: toString
};
var fromString = function (s) {
    if (s === "SIGABRT") {
        return new Data_Maybe.Just(SIGABRT.value);
    };
    if (s === "SIGALRM") {
        return new Data_Maybe.Just(SIGALRM.value);
    };
    if (s === "SIGBUS") {
        return new Data_Maybe.Just(SIGBUS.value);
    };
    if (s === "SIGCHLD") {
        return new Data_Maybe.Just(SIGCHLD.value);
    };
    if (s === "SIGCLD") {
        return new Data_Maybe.Just(SIGCLD.value);
    };
    if (s === "SIGCONT") {
        return new Data_Maybe.Just(SIGCONT.value);
    };
    if (s === "SIGEMT") {
        return new Data_Maybe.Just(SIGEMT.value);
    };
    if (s === "SIGFPE") {
        return new Data_Maybe.Just(SIGFPE.value);
    };
    if (s === "SIGHUP") {
        return new Data_Maybe.Just(SIGHUP.value);
    };
    if (s === "SIGILL") {
        return new Data_Maybe.Just(SIGILL.value);
    };
    if (s === "SIGINFO") {
        return new Data_Maybe.Just(SIGINFO.value);
    };
    if (s === "SIGINT") {
        return new Data_Maybe.Just(SIGINT.value);
    };
    if (s === "SIGIO") {
        return new Data_Maybe.Just(SIGIO.value);
    };
    if (s === "SIGIOT") {
        return new Data_Maybe.Just(SIGIOT.value);
    };
    if (s === "SIGKILL") {
        return new Data_Maybe.Just(SIGKILL.value);
    };
    if (s === "SIGLOST") {
        return new Data_Maybe.Just(SIGLOST.value);
    };
    if (s === "SIGPIPE") {
        return new Data_Maybe.Just(SIGPIPE.value);
    };
    if (s === "SIGPOLL") {
        return new Data_Maybe.Just(SIGPOLL.value);
    };
    if (s === "SIGPROF") {
        return new Data_Maybe.Just(SIGPROF.value);
    };
    if (s === "SIGPWR") {
        return new Data_Maybe.Just(SIGPWR.value);
    };
    if (s === "SIGQUIT") {
        return new Data_Maybe.Just(SIGQUIT.value);
    };
    if (s === "SIGSEGV") {
        return new Data_Maybe.Just(SIGSEGV.value);
    };
    if (s === "SIGSTKFLT") {
        return new Data_Maybe.Just(SIGSTKFLT.value);
    };
    if (s === "SIGSTOP") {
        return new Data_Maybe.Just(SIGSTOP.value);
    };
    if (s === "SIGSYS") {
        return new Data_Maybe.Just(SIGSYS.value);
    };
    if (s === "SIGTERM") {
        return new Data_Maybe.Just(SIGTERM.value);
    };
    if (s === "SIGTRAP") {
        return new Data_Maybe.Just(SIGTRAP.value);
    };
    if (s === "SIGTSTP") {
        return new Data_Maybe.Just(SIGTSTP.value);
    };
    if (s === "SIGTTIN") {
        return new Data_Maybe.Just(SIGTTIN.value);
    };
    if (s === "SIGTTOU") {
        return new Data_Maybe.Just(SIGTTOU.value);
    };
    if (s === "SIGUNUSED") {
        return new Data_Maybe.Just(SIGUNUSED.value);
    };
    if (s === "SIGURG") {
        return new Data_Maybe.Just(SIGURG.value);
    };
    if (s === "SIGUSR1") {
        return new Data_Maybe.Just(SIGUSR1.value);
    };
    if (s === "SIGUSR2") {
        return new Data_Maybe.Just(SIGUSR2.value);
    };
    if (s === "SIGVTALRM") {
        return new Data_Maybe.Just(SIGVTALRM.value);
    };
    if (s === "SIGWINCH") {
        return new Data_Maybe.Just(SIGWINCH.value);
    };
    if (s === "SIGXCPU") {
        return new Data_Maybe.Just(SIGXCPU.value);
    };
    if (s === "SIGXFSZ") {
        return new Data_Maybe.Just(SIGXFSZ.value);
    };
    return Data_Maybe.Nothing.value;
};
var eqSignal = {
    eq: function (x) {
        return function (y) {
            if (x instanceof SIGABRT && y instanceof SIGABRT) {
                return true;
            };
            if (x instanceof SIGALRM && y instanceof SIGALRM) {
                return true;
            };
            if (x instanceof SIGBUS && y instanceof SIGBUS) {
                return true;
            };
            if (x instanceof SIGCHLD && y instanceof SIGCHLD) {
                return true;
            };
            if (x instanceof SIGCLD && y instanceof SIGCLD) {
                return true;
            };
            if (x instanceof SIGCONT && y instanceof SIGCONT) {
                return true;
            };
            if (x instanceof SIGEMT && y instanceof SIGEMT) {
                return true;
            };
            if (x instanceof SIGFPE && y instanceof SIGFPE) {
                return true;
            };
            if (x instanceof SIGHUP && y instanceof SIGHUP) {
                return true;
            };
            if (x instanceof SIGILL && y instanceof SIGILL) {
                return true;
            };
            if (x instanceof SIGINFO && y instanceof SIGINFO) {
                return true;
            };
            if (x instanceof SIGINT && y instanceof SIGINT) {
                return true;
            };
            if (x instanceof SIGIO && y instanceof SIGIO) {
                return true;
            };
            if (x instanceof SIGIOT && y instanceof SIGIOT) {
                return true;
            };
            if (x instanceof SIGKILL && y instanceof SIGKILL) {
                return true;
            };
            if (x instanceof SIGLOST && y instanceof SIGLOST) {
                return true;
            };
            if (x instanceof SIGPIPE && y instanceof SIGPIPE) {
                return true;
            };
            if (x instanceof SIGPOLL && y instanceof SIGPOLL) {
                return true;
            };
            if (x instanceof SIGPROF && y instanceof SIGPROF) {
                return true;
            };
            if (x instanceof SIGPWR && y instanceof SIGPWR) {
                return true;
            };
            if (x instanceof SIGQUIT && y instanceof SIGQUIT) {
                return true;
            };
            if (x instanceof SIGSEGV && y instanceof SIGSEGV) {
                return true;
            };
            if (x instanceof SIGSTKFLT && y instanceof SIGSTKFLT) {
                return true;
            };
            if (x instanceof SIGSTOP && y instanceof SIGSTOP) {
                return true;
            };
            if (x instanceof SIGSYS && y instanceof SIGSYS) {
                return true;
            };
            if (x instanceof SIGTERM && y instanceof SIGTERM) {
                return true;
            };
            if (x instanceof SIGTRAP && y instanceof SIGTRAP) {
                return true;
            };
            if (x instanceof SIGTSTP && y instanceof SIGTSTP) {
                return true;
            };
            if (x instanceof SIGTTIN && y instanceof SIGTTIN) {
                return true;
            };
            if (x instanceof SIGTTOU && y instanceof SIGTTOU) {
                return true;
            };
            if (x instanceof SIGUNUSED && y instanceof SIGUNUSED) {
                return true;
            };
            if (x instanceof SIGURG && y instanceof SIGURG) {
                return true;
            };
            if (x instanceof SIGUSR1 && y instanceof SIGUSR1) {
                return true;
            };
            if (x instanceof SIGUSR2 && y instanceof SIGUSR2) {
                return true;
            };
            if (x instanceof SIGVTALRM && y instanceof SIGVTALRM) {
                return true;
            };
            if (x instanceof SIGWINCH && y instanceof SIGWINCH) {
                return true;
            };
            if (x instanceof SIGXCPU && y instanceof SIGXCPU) {
                return true;
            };
            if (x instanceof SIGXFSZ && y instanceof SIGXFSZ) {
                return true;
            };
            return false;
        };
    }
};
var ordSignal = {
    compare: function (x) {
        return function (y) {
            if (x instanceof SIGABRT && y instanceof SIGABRT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGABRT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGABRT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGALRM && y instanceof SIGALRM) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGALRM) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGALRM) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGBUS && y instanceof SIGBUS) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGBUS) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGBUS) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGCHLD && y instanceof SIGCHLD) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGCHLD) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGCHLD) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGCLD && y instanceof SIGCLD) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGCLD) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGCLD) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGCONT && y instanceof SIGCONT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGCONT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGCONT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGEMT && y instanceof SIGEMT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGEMT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGEMT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGFPE && y instanceof SIGFPE) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGFPE) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGFPE) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGHUP && y instanceof SIGHUP) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGHUP) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGHUP) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGILL && y instanceof SIGILL) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGILL) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGILL) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGINFO && y instanceof SIGINFO) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGINFO) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGINFO) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGINT && y instanceof SIGINT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGINT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGINT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGIO && y instanceof SIGIO) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGIO) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGIO) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGIOT && y instanceof SIGIOT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGIOT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGIOT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGKILL && y instanceof SIGKILL) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGKILL) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGKILL) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGLOST && y instanceof SIGLOST) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGLOST) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGLOST) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGPIPE && y instanceof SIGPIPE) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGPIPE) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGPIPE) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGPOLL && y instanceof SIGPOLL) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGPOLL) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGPOLL) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGPROF && y instanceof SIGPROF) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGPROF) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGPROF) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGPWR && y instanceof SIGPWR) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGPWR) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGPWR) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGQUIT && y instanceof SIGQUIT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGQUIT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGQUIT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGSEGV && y instanceof SIGSEGV) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGSEGV) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGSEGV) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGSTKFLT && y instanceof SIGSTKFLT) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGSTKFLT) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGSTKFLT) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGSTOP && y instanceof SIGSTOP) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGSTOP) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGSTOP) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGSYS && y instanceof SIGSYS) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGSYS) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGSYS) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGTERM && y instanceof SIGTERM) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGTERM) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGTERM) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGTRAP && y instanceof SIGTRAP) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGTRAP) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGTRAP) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGTSTP && y instanceof SIGTSTP) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGTSTP) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGTSTP) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGTTIN && y instanceof SIGTTIN) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGTTIN) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGTTIN) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGTTOU && y instanceof SIGTTOU) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGTTOU) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGTTOU) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGUNUSED && y instanceof SIGUNUSED) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGUNUSED) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGUNUSED) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGURG && y instanceof SIGURG) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGURG) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGURG) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGUSR1 && y instanceof SIGUSR1) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGUSR1) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGUSR1) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGUSR2 && y instanceof SIGUSR2) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGUSR2) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGUSR2) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGVTALRM && y instanceof SIGVTALRM) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGVTALRM) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGVTALRM) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGWINCH && y instanceof SIGWINCH) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGWINCH) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGWINCH) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGXCPU && y instanceof SIGXCPU) {
                return Data_Ordering.EQ.value;
            };
            if (x instanceof SIGXCPU) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof SIGXCPU) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof SIGXFSZ && y instanceof SIGXFSZ) {
                return Data_Ordering.EQ.value;
            };
            throw new Error("Failed pattern match at Data.Posix.Signal (line 140, column 1 - line 140, column 40): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqSignal;
    }
};
module.exports = {
    SIGABRT: SIGABRT,
    SIGALRM: SIGALRM,
    SIGBUS: SIGBUS,
    SIGCHLD: SIGCHLD,
    SIGCLD: SIGCLD,
    SIGCONT: SIGCONT,
    SIGEMT: SIGEMT,
    SIGFPE: SIGFPE,
    SIGHUP: SIGHUP,
    SIGILL: SIGILL,
    SIGINFO: SIGINFO,
    SIGINT: SIGINT,
    SIGIO: SIGIO,
    SIGIOT: SIGIOT,
    SIGKILL: SIGKILL,
    SIGLOST: SIGLOST,
    SIGPIPE: SIGPIPE,
    SIGPOLL: SIGPOLL,
    SIGPROF: SIGPROF,
    SIGPWR: SIGPWR,
    SIGQUIT: SIGQUIT,
    SIGSEGV: SIGSEGV,
    SIGSTKFLT: SIGSTKFLT,
    SIGSTOP: SIGSTOP,
    SIGSYS: SIGSYS,
    SIGTERM: SIGTERM,
    SIGTRAP: SIGTRAP,
    SIGTSTP: SIGTSTP,
    SIGTTIN: SIGTTIN,
    SIGTTOU: SIGTTOU,
    SIGUNUSED: SIGUNUSED,
    SIGURG: SIGURG,
    SIGUSR1: SIGUSR1,
    SIGUSR2: SIGUSR2,
    SIGVTALRM: SIGVTALRM,
    SIGWINCH: SIGWINCH,
    SIGXCPU: SIGXCPU,
    SIGXFSZ: SIGXFSZ,
    toString: toString,
    fromString: fromString,
    showSignal: showSignal,
    eqSignal: eqSignal,
    ordSignal: ordSignal
};
