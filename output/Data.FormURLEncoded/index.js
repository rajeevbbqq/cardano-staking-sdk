// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_Common = require("../Data.String.Common/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var JSURI = require("../JSURI/index.js");
var FormURLEncoded = function (x) {
    return x;
};
var toArray = function (v) {
    return v;
};
var showFormUrlEncoded = {
    show: function (v) {
        return "(FormURLEncoded " + (Data_Show.show(Data_Show.showArray(Data_Tuple.showTuple(Data_Show.showString)(Data_Maybe.showMaybe(Data_Show.showString))))(v) + ")");
    }
};
var semigroupFormUrlEncoded = Data_Semigroup.semigroupArray;
var ordFormUrlEncoded = Data_Ord.ordArray(Data_Tuple.ordTuple(Data_Ord.ordString)(Data_Maybe.ordMaybe(Data_Ord.ordString)));
var newtypeFormUrlEncoded = {
    Coercible0: function () {
        return undefined;
    }
};
var monoidFormUrlEncoded = Data_Monoid.monoidArray;
var fromArray = FormURLEncoded;
var eqFormUrlEncoded = Data_Eq.eqArray(Data_Tuple.eqTuple(Data_Eq.eqString)(Data_Maybe.eqMaybe(Data_Eq.eqString)));
var encode = (function () {
    var encodePart = function (v) {
        if (v.value1 instanceof Data_Maybe.Nothing) {
            return JSURI.encodeFormURLComponent(v.value0);
        };
        if (v.value1 instanceof Data_Maybe.Just) {
            return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (key) {
                return function (val) {
                    return key + ("=" + val);
                };
            })(JSURI.encodeFormURLComponent(v.value0)))(JSURI.encodeFormURLComponent(v.value1.value0));
        };
        throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 18 - line 39, column 116): " + [ v.constructor.name ]);
    };
    var $16 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_String_Common.joinWith("&"));
    var $17 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(encodePart);
    return function ($18) {
        return $16($17(toArray($18)));
    };
})();
var decode = (function () {
    var decodePart = (function () {
        var $19 = Data_String_Common.split("=");
        return function ($20) {
            return (function (v) {
                if (v.length === 2) {
                    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(function (key) {
                        return function (val) {
                            return Data_Tuple.Tuple.create(key)(new Data_Maybe.Just(val));
                        };
                    })(JSURI.decodeFormURLComponent(v[0])))(JSURI.decodeFormURLComponent(v[1]));
                };
                if (v.length === 1) {
                    return Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Tuple.Tuple.create)(JSURI.decodeFormURLComponent(v[0])))(Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value));
                };
                return Data_Maybe.Nothing.value;
            })($19($20));
        };
    })();
    var $21 = Data_Functor.map(Data_Maybe.functorMaybe)(FormURLEncoded);
    var $22 = Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(decodePart);
    var $23 = Data_String_Common.split("&");
    return function ($24) {
        return $21($22($23($24)));
    };
})();
module.exports = {
    FormURLEncoded: FormURLEncoded,
    fromArray: fromArray,
    toArray: toArray,
    encode: encode,
    decode: decode,
    newtypeFormUrlEncoded: newtypeFormUrlEncoded,
    eqFormUrlEncoded: eqFormUrlEncoded,
    ordFormUrlEncoded: ordFormUrlEncoded,
    semigroupFormUrlEncoded: semigroupFormUrlEncoded,
    monoidFormUrlEncoded: monoidFormUrlEncoded,
    showFormUrlEncoded: showFormUrlEncoded
};
