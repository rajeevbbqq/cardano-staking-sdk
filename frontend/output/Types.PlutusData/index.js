// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Types_ByteArray = require("../Types.ByteArray/index.js");
var Constr = (function () {
    function Constr(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    Constr.create = function (value0) {
        return function (value1) {
            return new Constr(value0, value1);
        };
    };
    return Constr;
})();
var $$Map = (function () {
    function $$Map(value0) {
        this.value0 = value0;
    };
    $$Map.create = function (value0) {
        return new $$Map(value0);
    };
    return $$Map;
})();
var List = (function () {
    function List(value0) {
        this.value0 = value0;
    };
    List.create = function (value0) {
        return new List(value0);
    };
    return List;
})();
var Integer = (function () {
    function Integer(value0) {
        this.value0 = value0;
    };
    Integer.create = function (value0) {
        return new Integer(value0);
    };
    return Integer;
})();
var Bytes = (function () {
    function Bytes(value0) {
        this.value0 = value0;
    };
    Bytes.create = function (value0) {
        return new Bytes(value0);
    };
    return Bytes;
})();
var genericPlutusData_ = {
    to: function (x) {
        if (x instanceof Data_Generic_Rep.Inl) {
            return new Constr(x.value0.value0, x.value0.value1);
        };
        if (x instanceof Data_Generic_Rep.Inr && x.value0 instanceof Data_Generic_Rep.Inl) {
            return new $$Map(x.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0 instanceof Data_Generic_Rep.Inl)) {
            return new List(x.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inl))) {
            return new Integer(x.value0.value0.value0.value0);
        };
        if (x instanceof Data_Generic_Rep.Inr && (x.value0 instanceof Data_Generic_Rep.Inr && (x.value0.value0 instanceof Data_Generic_Rep.Inr && x.value0.value0.value0 instanceof Data_Generic_Rep.Inr))) {
            return new Bytes(x.value0.value0.value0.value0);
        };
        throw new Error("Failed pattern match at Types.PlutusData (line 44, column 1 - line 44, column 37): " + [ x.constructor.name ]);
    },
    from: function (x) {
        if (x instanceof Constr) {
            return new Data_Generic_Rep.Inl(new Data_Generic_Rep.Product(x.value0, x.value1));
        };
        if (x instanceof $$Map) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0));
        };
        if (x instanceof List) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0)));
        };
        if (x instanceof Integer) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inl(x.value0))));
        };
        if (x instanceof Bytes) {
            return new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(new Data_Generic_Rep.Inr(x.value0))));
        };
        throw new Error("Failed pattern match at Types.PlutusData (line 44, column 1 - line 44, column 37): " + [ x.constructor.name ]);
    }
};
var showPlutusData = {
    show: function (x) {
        return Data_Show_Generic.genericShow(genericPlutusData_)(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsProduct(Data_Show_Generic.genericShowArgsArgument(Data_BigInt.showBigInt))(Data_Show_Generic.genericShowArgsArgument(Data_Show.showArray(showPlutusData))))({
            reflectSymbol: function () {
                return "Constr";
            }
        }))(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showArray(Data_Tuple.showTuple(showPlutusData)(showPlutusData))))({
            reflectSymbol: function () {
                return "Map";
            }
        }))(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showArray(showPlutusData)))({
            reflectSymbol: function () {
                return "List";
            }
        }))(Data_Show_Generic.genericShowSum(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_BigInt.showBigInt))({
            reflectSymbol: function () {
                return "Integer";
            }
        }))(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Types_ByteArray.showByteArray))({
            reflectSymbol: function () {
                return "Bytes";
            }
        }))))))(x);
    }
};
var eqPlutusData = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Constr && y instanceof Constr) {
                return Data_Eq.eq(Data_BigInt.eqBigInt)(x.value0)(y.value0) && Data_Eq.eq(Data_Eq.eqArray(eqPlutusData))(x.value1)(y.value1);
            };
            if (x instanceof $$Map && y instanceof $$Map) {
                return Data_Eq.eq(Data_Eq.eqArray(Data_Tuple.eqTuple(eqPlutusData)(eqPlutusData)))(x.value0)(y.value0);
            };
            if (x instanceof List && y instanceof List) {
                return Data_Eq.eq(Data_Eq.eqArray(eqPlutusData))(x.value0)(y.value0);
            };
            if (x instanceof Integer && y instanceof Integer) {
                return Data_Eq.eq(Data_BigInt.eqBigInt)(x.value0)(y.value0);
            };
            if (x instanceof Bytes && y instanceof Bytes) {
                return Data_Eq.eq(Types_ByteArray.eqByteArray)(x.value0)(y.value0);
            };
            return false;
        };
    }
};
var ordPlutusData = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Constr && y instanceof Constr) {
                var v = Data_Ord.compare(Data_BigInt.ordBigInt)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(Data_Ord.ordArray(ordPlutusData))(x.value1)(y.value1);
            };
            if (x instanceof Constr) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Constr) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof $$Map && y instanceof $$Map) {
                return Data_Ord.compare(Data_Ord.ordArray(Data_Tuple.ordTuple(ordPlutusData)(ordPlutusData)))(x.value0)(y.value0);
            };
            if (x instanceof $$Map) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof $$Map) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof List && y instanceof List) {
                return Data_Ord.compare(Data_Ord.ordArray(ordPlutusData))(x.value0)(y.value0);
            };
            if (x instanceof List) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof List) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Integer && y instanceof Integer) {
                return Data_Ord.compare(Data_BigInt.ordBigInt)(x.value0)(y.value0);
            };
            if (x instanceof Integer) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Integer) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof Bytes && y instanceof Bytes) {
                return Data_Ord.compare(Types_ByteArray.ordByteArray)(x.value0)(y.value0);
            };
            throw new Error("Failed pattern match at Types.PlutusData (line 43, column 1 - line 43, column 31): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqPlutusData;
    }
};
var encodeAesonPlutusData = {
    "encodeAeson'": function (v) {
        if (v instanceof Constr) {
            return Aeson["encodeAeson'"](Aeson.encodeAesonRecord(Aeson.gEncodeAesonCons(Aeson.encodeAesonAeson)(Aeson.gEncodeAesonCons(Aeson.encodeAesonAeson)(Aeson.gEncodeAesonNil)({
                reflectSymbol: function () {
                    return "fields";
                }
            })())({
                reflectSymbol: function () {
                    return "constr";
                }
            })())())({
                constr: Aeson.encodeAeson(Aeson.encodeAesonBigInt)(v.value0),
                fields: Aeson.encodeAeson(Aeson.encodeAesonArray(encodeAesonPlutusData))(v.value1)
            });
        };
        if (v instanceof $$Map) {
            return Aeson["encodeAeson'"](Aeson.encodeAesonRecord(Aeson.gEncodeAesonCons(Aeson.encodeAesonAeson)(Aeson.gEncodeAesonNil)({
                reflectSymbol: function () {
                    return "map";
                }
            })())())({
                map: Aeson.encodeAeson(Aeson.encodeAesonArray(Aeson.encodeAesonRecord(Aeson.gEncodeAesonCons(Aeson.encodeAesonAeson)(Aeson.gEncodeAesonCons(Aeson.encodeAesonAeson)(Aeson.gEncodeAesonNil)({
                    reflectSymbol: function () {
                        return "value";
                    }
                })())({
                    reflectSymbol: function () {
                        return "key";
                    }
                })())()))(Data_Functor.map(Data_Functor.functorArray)(function (v1) {
                    return {
                        key: Aeson.encodeAeson(encodeAesonPlutusData)(v1.value0),
                        value: Aeson.encodeAeson(encodeAesonPlutusData)(v1.value1)
                    };
                })(v.value0))
            });
        };
        if (v instanceof List) {
            return Aeson["encodeAeson'"](Aeson.encodeAesonArray(encodeAesonPlutusData))(v.value0);
        };
        if (v instanceof Integer) {
            return Aeson["encodeAeson'"](Aeson.encodeAesonBigInt)(v.value0);
        };
        if (v instanceof Bytes) {
            return Aeson["encodeAeson'"](Types_ByteArray.encodeAesonByteArray)(v.value0);
        };
        throw new Error("Failed pattern match at Types.PlutusData (line 90, column 1 - line 106, column 44): " + [ v.constructor.name ]);
    }
};
var decodeAesonPlutusData = {
    decodeAeson: function (aeson) {
        var decodeMap = Control_Bind.bind(Data_Either.bindEither)(Aeson.decodeAeson(Aeson.decodeAesonObject(Aeson.decodeAesonAeson))(aeson))(function (obj) {
            return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonArray(Aeson.decodeAesonObject(Aeson.decodeAesonAeson)))(obj)("map"))(function (map1) {
                return Control_Bind.bind(Data_Either.bindEither)(Data_Traversable["for"](Data_Either.applicativeEither)(Data_Traversable.traversableArray)(map1)(function (entryJson) {
                    return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(decodeAesonPlutusData)(entryJson)("key"))(function (key) {
                        return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(decodeAesonPlutusData)(entryJson)("value"))(function (value) {
                            return Control_Applicative.pure(Data_Either.applicativeEither)(new Data_Tuple.Tuple(key, value));
                        });
                    });
                }))(function (kvs) {
                    return Control_Applicative.pure(Data_Either.applicativeEither)(new $$Map(kvs));
                });
            });
        });
        var decodeList = Data_Functor.map(Data_Either.functorEither)(List.create)(Aeson.decodeAeson(Aeson.decodeAesonArray(decodeAesonPlutusData))(aeson));
        var decodeInteger = Data_Functor.map(Data_Either.functorEither)(Integer.create)(Aeson.decodeAeson(Aeson.decodeAesonBigInt)(aeson));
        var decodeConstr = Control_Bind.bind(Data_Either.bindEither)(Aeson.decodeAeson(Aeson.decodeAesonObject(Aeson.decodeAesonAeson))(aeson))(function (x) {
            return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonBigInt)(x)("constr"))(function (constr) {
                return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonArray(decodeAesonPlutusData))(x)("fields"))(function (fields) {
                    return Control_Applicative.pure(Data_Either.applicativeEither)(new Constr(constr, fields));
                });
            });
        });
        var decodeBytes = Control_Bind.bind(Data_Either.bindEither)(Aeson.decodeAeson(Aeson.decodeAesonString)(aeson))(function (bytesHex) {
            var v = Types_ByteArray.hexToByteArray(bytesHex);
            if (v instanceof Data_Maybe.Nothing) {
                return Data_Either.Left.create(Data_Argonaut_Decode_Error.UnexpectedValue.create(Aeson.toStringifiedNumbersJson(Aeson.encodeAeson(Aeson.encodeAesonString)(bytesHex))));
            };
            if (v instanceof Data_Maybe.Just) {
                return Control_Applicative.pure(Data_Either.applicativeEither)(new Bytes(v.value0));
            };
            throw new Error("Failed pattern match at Types.PlutusData (line 85, column 7 - line 88, column 37): " + [ v.constructor.name ]);
        });
        return Control_Alt.alt(Data_Either.altEither)(Control_Alt.alt(Data_Either.altEither)(Control_Alt.alt(Data_Either.altEither)(Control_Alt.alt(Data_Either.altEither)(decodeConstr)(decodeMap))(decodeList))(decodeInteger))(decodeBytes);
    }
};
module.exports = {
    Constr: Constr,
    "Map": $$Map,
    List: List,
    Integer: Integer,
    Bytes: Bytes,
    eqPlutusData: eqPlutusData,
    ordPlutusData: ordPlutusData,
    genericPlutusData_: genericPlutusData_,
    showPlutusData: showPlutusData,
    decodeAesonPlutusData: decodeAesonPlutusData,
    encodeAesonPlutusData: encodeAesonPlutusData
};
