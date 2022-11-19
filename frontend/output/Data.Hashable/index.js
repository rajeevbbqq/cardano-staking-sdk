// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Enum = require("../Data.Enum/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Symbol = require("../Data.Symbol/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Record = require("../Record/index.js");
var Type_Data_RowList = require("../Type.Data.RowList/index.js");
var hashableVoid = {
    hash: function (v) {
        return 0;
    },
    Eq0: function () {
        return Data_Eq.eqVoid;
    }
};
var hashableUnit = {
    hash: function (v) {
        return 1;
    },
    Eq0: function () {
        return Data_Eq.eqUnit;
    }
};
var hashableString = {
    hash: $foreign.hashString,
    Eq0: function () {
        return Data_Eq.eqString;
    }
};
var hashableRecordNil = {
    hashRecord: function (v) {
        return function (v1) {
            return 0;
        };
    },
    EqRecord0: function () {
        return Data_Eq.eqRowNil;
    }
};
var hashableNumber = {
    hash: $foreign.hashNumber,
    Eq0: function () {
        return Data_Eq.eqNumber;
    }
};
var hashableInt = {
    hash: function (n) {
        return n;
    },
    Eq0: function () {
        return Data_Eq.eqInt;
    }
};
var hashableChar = {
    hash: Data_Enum.fromEnum(Data_Enum.boundedEnumChar),
    Eq0: function () {
        return Data_Eq.eqChar;
    }
};
var hashableBoolean = {
    hash: function (v) {
        if (!v) {
            return 0;
        };
        if (v) {
            return 1;
        };
        throw new Error("Failed pattern match at Data.Hashable (line 52, column 1 - line 54, column 16): " + [ v.constructor.name ]);
    },
    Eq0: function () {
        return Data_Eq.eqBoolean;
    }
};
var hashRecord = function (dict) {
    return dict.hashRecord;
};
var hashableRecord = function (dictRowToList) {
    return function (dictHashableRecord) {
        return function (dictEqRecord) {
            return {
                hash: hashRecord(dictHashableRecord)(Type_Data_RowList.RLProxy.value),
                Eq0: function () {
                    return Data_Eq.eqRec()(dictEqRecord);
                }
            };
        };
    };
};
var hash = function (dict) {
    return dict.hash;
};
var hashFoldable = function (dictFoldable) {
    return function (dictHashable) {
        return Data_Foldable.foldl(dictFoldable)(function (h) {
            return function (a) {
                return (31 * h | 0) + hash(dictHashable)(a) | 0;
            };
        })(1);
    };
};
var hashableArray = function (dictHashable) {
    return {
        hash: hashFoldable(Data_Foldable.foldableArray)(dictHashable),
        Eq0: function () {
            return Data_Eq.eqArray(dictHashable.Eq0());
        }
    };
};
var hashableList = function (dictHashable) {
    return {
        hash: hashFoldable(Data_List_Types.foldableList)(dictHashable),
        Eq0: function () {
            return Data_List_Types.eqList(dictHashable.Eq0());
        }
    };
};
var hashableEither = function (dictHashable) {
    return function (dictHashable1) {
        return {
            hash: function (v) {
                if (v instanceof Data_Either.Left) {
                    return hash(dictHashable)(v.value0) ^ 1431655765;
                };
                if (v instanceof Data_Either.Right) {
                    return hash(dictHashable1)(v.value0);
                };
                throw new Error("Failed pattern match at Data.Hashable (line 88, column 1 - line 90, column 26): " + [ v.constructor.name ]);
            },
            Eq0: function () {
                return Data_Either.eqEither(dictHashable.Eq0())(dictHashable1.Eq0());
            }
        };
    };
};
var hashableMaybe = function (dictHashable) {
    return {
        hash: function (v) {
            if (v instanceof Data_Maybe.Nothing) {
                return 0;
            };
            if (v instanceof Data_Maybe.Just) {
                return 1 + hash(dictHashable)(v.value0) | 0;
            };
            throw new Error("Failed pattern match at Data.Hashable (line 84, column 1 - line 86, column 29): " + [ v.constructor.name ]);
        },
        Eq0: function () {
            return Data_Maybe.eqMaybe(dictHashable.Eq0());
        }
    };
};
var hashableRecordCons = function (dictHashable) {
    return function (dictHashableRecord) {
        return function (dictIsSymbol) {
            return function (dictCons) {
                return {
                    hashRecord: function (v) {
                        return function (record) {
                            return (hash(dictHashable)(Record.get(dictIsSymbol)()(Data_Symbol.SProxy.value)(record)) * 31 | 0) + hashRecord(dictHashableRecord)(Type_Data_RowList.RLProxy.value)(record) | 0;
                        };
                    },
                    EqRecord0: function () {
                        return Data_Eq.eqRowCons(dictHashableRecord.EqRecord0())()(dictIsSymbol)(dictHashable.Eq0());
                    }
                };
            };
        };
    };
};
var hashableTuple = function (dictHashable) {
    return function (dictHashable1) {
        return {
            hash: function (v) {
                return (hash(dictHashable)(v.value0) * 31 | 0) + hash(dictHashable1)(v.value1) | 0;
            },
            Eq0: function () {
                return Data_Tuple.eqTuple(dictHashable.Eq0())(dictHashable1.Eq0());
            }
        };
    };
};
module.exports = {
    hash: hash,
    hashRecord: hashRecord,
    hashableBoolean: hashableBoolean,
    hashableInt: hashableInt,
    hashableNumber: hashableNumber,
    hashableChar: hashableChar,
    hashableString: hashableString,
    hashableArray: hashableArray,
    hashableList: hashableList,
    hashableTuple: hashableTuple,
    hashableMaybe: hashableMaybe,
    hashableEither: hashableEither,
    hashableUnit: hashableUnit,
    hashableVoid: hashableVoid,
    hashableRecordNil: hashableRecordNil,
    hashableRecordCons: hashableRecordCons,
    hashableRecord: hashableRecord
};