// Generated by purs version 0.14.5
"use strict";
var Data_Show = require("../Data.Show/index.js");
var Data_Void = require("../Data.Void/index.js");
var Test_Spec_Result = require("../Test.Spec.Result/index.js");
var Test_Spec_Tree = require("../Test.Spec.Tree/index.js");
var Parallel = (function () {
    function Parallel() {

    };
    Parallel.value = new Parallel();
    return Parallel;
})();
var Sequential = (function () {
    function Sequential() {

    };
    Sequential.value = new Sequential();
    return Sequential;
})();
var Start = (function () {
    function Start(value0) {
        this.value0 = value0;
    };
    Start.create = function (value0) {
        return new Start(value0);
    };
    return Start;
})();
var Suite = (function () {
    function Suite(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Suite.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Suite(value0, value1, value2);
            };
        };
    };
    return Suite;
})();
var SuiteEnd = (function () {
    function SuiteEnd(value0) {
        this.value0 = value0;
    };
    SuiteEnd.create = function (value0) {
        return new SuiteEnd(value0);
    };
    return SuiteEnd;
})();
var Test = (function () {
    function Test(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    Test.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new Test(value0, value1, value2);
            };
        };
    };
    return Test;
})();
var TestEnd = (function () {
    function TestEnd(value0, value1, value2) {
        this.value0 = value0;
        this.value1 = value1;
        this.value2 = value2;
    };
    TestEnd.create = function (value0) {
        return function (value1) {
            return function (value2) {
                return new TestEnd(value0, value1, value2);
            };
        };
    };
    return TestEnd;
})();
var Pending = (function () {
    function Pending(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Pending.create = function (value0) {
        return function (value1) {
            return new Pending(value0, value1);
        };
    };
    return Pending;
})();
var End = (function () {
    function End(value0) {
        this.value0 = value0;
    };
    End.create = function (value0) {
        return new End(value0);
    };
    return End;
})();
var showExecution = {
    show: function (v) {
        if (v instanceof Parallel) {
            return "Parallel";
        };
        if (v instanceof Sequential) {
            return "Sequential";
        };
        throw new Error("Failed pattern match at Test.Spec.Runner.Event (line 14, column 10 - line 16, column 31): " + [ v.constructor.name ]);
    }
};
var showEvent = {
    show: function (v) {
        if (v instanceof Start) {
            return "Start " + Data_Show.show(Data_Show.showInt)(v.value0);
        };
        if (v instanceof Suite) {
            return "Suite " + (Data_Show.show(showExecution)(v.value0) + (Data_Show.show(Data_Show.showArray(Test_Spec_Tree.showIdTerm))(v.value1) + (": " + v.value2)));
        };
        if (v instanceof SuiteEnd) {
            return "SuiteEnd " + Data_Show.show(Data_Show.showArray(Test_Spec_Tree.showIdTerm))(v.value0);
        };
        if (v instanceof Test) {
            return "Test " + (Data_Show.show(showExecution)(v.value0) + (Data_Show.show(Data_Show.showArray(Test_Spec_Tree.showIdTerm))(v.value1) + (" " + v.value2)));
        };
        if (v instanceof TestEnd) {
            return "TestEnd " + (Data_Show.show(Data_Show.showArray(Test_Spec_Tree.showIdTerm))(v.value0) + (" " + (v.value1 + (": " + Data_Show.show(Test_Spec_Result.showResult)(v.value2)))));
        };
        if (v instanceof Pending) {
            return "Pending " + (Data_Show.show(Data_Show.showArray(Test_Spec_Tree.showIdTerm))(v.value0) + (" " + v.value1));
        };
        if (v instanceof End) {
            return "End " + Data_Show.show(Data_Show.showArray(Test_Spec_Tree.showGroup(Data_Void.showVoid)(Test_Spec_Result.showResult)))(v.value0);
        };
        throw new Error("Failed pattern match at Test.Spec.Runner.Event (line 28, column 10 - line 35, column 42): " + [ v.constructor.name ]);
    }
};
module.exports = {
    Parallel: Parallel,
    Sequential: Sequential,
    Start: Start,
    Suite: Suite,
    SuiteEnd: SuiteEnd,
    Test: Test,
    TestEnd: TestEnd,
    Pending: Pending,
    End: End,
    showExecution: showExecution,
    showEvent: showEvent
};
