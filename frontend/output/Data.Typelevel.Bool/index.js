// Generated by purs version 0.14.5
"use strict";
var Data_Typelevel_Undefined = require("../Data.Typelevel.Undefined/index.js");
var xor = function (dictXor) {
    return Data_Typelevel_Undefined["undefined"];
};
var trueT = Data_Typelevel_Undefined["undefined"];
var toBool = function (dict) {
    return dict.toBool;
};
var showTrue = {
    show: function (v) {
        return "True";
    }
};
var showFalse = {
    show: function (v) {
        return "False";
    }
};
var or = function (dictOr) {
    return Data_Typelevel_Undefined["undefined"];
};
var not = function (dictNot) {
    return Data_Typelevel_Undefined["undefined"];
};
var imp = function (dictImp) {
    return Data_Typelevel_Undefined["undefined"];
};
var falseT = Data_Typelevel_Undefined["undefined"];
var eq = function (dictEq) {
    return Data_Typelevel_Undefined["undefined"];
};
var boolITrue = {
    toBool: function (v) {
        return true;
    }
};
var eqTrueTrue = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var impTrueTrue = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var orTrueTrue = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var boolIFalse = {
    toBool: function (v) {
        return false;
    }
};
var eqFalseFalse = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var eqFalseTrue = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var eqTrueFalse = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var impFalseFalse = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var impFalseTrue = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var impTrueFalse = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var notFalse = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolITrue;
    }
};
var notTrue = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolIFalse;
    }
};
var orFalseFalse = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var orFalseTrue = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var orTrueFalse = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var xorFalseFalse = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var xorFalseTrue = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var xorTrueFalse = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var xorTrueTrue = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var boolIBool = function (dictBoolI) {
    return {
        BoolI0: function () {
            return dictBoolI;
        }
    };
};
var reifyBool = function (v) {
    return function (f) {
        if (v) {
            return f(boolIBool(boolITrue))(trueT);
        };
        if (!v) {
            return f(boolIBool(boolIFalse))(falseT);
        };
        throw new Error("Failed pattern match at Data.Typelevel.Bool (line 63, column 1 - line 63, column 68): " + [ v.constructor.name, f.constructor.name ]);
    };
};
var andTrueTrue = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolITrue;
    }
};
var andTrueFalse = {
    BoolI0: function () {
        return boolITrue;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var andFalseTrue = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolITrue;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var andFalseFalse = {
    BoolI0: function () {
        return boolIFalse;
    },
    BoolI1: function () {
        return boolIFalse;
    },
    BoolI2: function () {
        return boolIFalse;
    }
};
var and = function (dictAnd) {
    return Data_Typelevel_Undefined["undefined"];
};
module.exports = {
    trueT: trueT,
    falseT: falseT,
    reifyBool: reifyBool,
    toBool: toBool,
    not: not,
    and: and,
    or: or,
    xor: xor,
    imp: imp,
    eq: eq,
    showTrue: showTrue,
    showFalse: showFalse,
    boolIBool: boolIBool,
    boolITrue: boolITrue,
    boolIFalse: boolIFalse,
    notFalse: notFalse,
    notTrue: notTrue,
    andFalseFalse: andFalseFalse,
    andFalseTrue: andFalseTrue,
    andTrueFalse: andTrueFalse,
    andTrueTrue: andTrueTrue,
    orFalseFalse: orFalseFalse,
    orFalseTrue: orFalseTrue,
    orTrueFalse: orTrueFalse,
    orTrueTrue: orTrueTrue,
    xorFalseFalse: xorFalseFalse,
    xorFalseTrue: xorFalseTrue,
    xorTrueFalse: xorTrueFalse,
    xorTrueTrue: xorTrueTrue,
    impFalseFalse: impFalseFalse,
    impFalseTrue: impFalseTrue,
    impTrueFalse: impTrueFalse,
    impTrueTrue: impTrueTrue,
    eqFalseFalse: eqFalseFalse,
    eqFalseTrue: eqFalseTrue,
    eqTrueFalse: eqTrueFalse,
    eqTrueTrue: eqTrueTrue
};
