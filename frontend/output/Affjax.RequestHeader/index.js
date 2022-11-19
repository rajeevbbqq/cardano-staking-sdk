// Generated by purs version 0.14.5
"use strict";
var Data_Eq = require("../Data.Eq/index.js");
var Data_MediaType = require("../Data.MediaType/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Show = require("../Data.Show/index.js");
var Accept = (function () {
    function Accept(value0) {
        this.value0 = value0;
    };
    Accept.create = function (value0) {
        return new Accept(value0);
    };
    return Accept;
})();
var ContentType = (function () {
    function ContentType(value0) {
        this.value0 = value0;
    };
    ContentType.create = function (value0) {
        return new ContentType(value0);
    };
    return ContentType;
})();
var RequestHeader = (function () {
    function RequestHeader(value0, value1) {
        this.value0 = value0;
        this.value1 = value1;
    };
    RequestHeader.create = function (value0) {
        return function (value1) {
            return new RequestHeader(value0, value1);
        };
    };
    return RequestHeader;
})();
var value = function (v) {
    if (v instanceof Accept) {
        return Data_Newtype.unwrap()(v.value0);
    };
    if (v instanceof ContentType) {
        return Data_Newtype.unwrap()(v.value0);
    };
    if (v instanceof RequestHeader) {
        return v.value1;
    };
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [ v.constructor.name ]);
};
var showRequestHeader = {
    show: function (v) {
        if (v instanceof Accept) {
            return "(Accept " + (Data_Show.show(Data_MediaType.showMediaType)(v.value0) + ")");
        };
        if (v instanceof ContentType) {
            return "(ContentType " + (Data_Show.show(Data_MediaType.showMediaType)(v.value0) + ")");
        };
        if (v instanceof RequestHeader) {
            return "(RequestHeader " + (Data_Show.show(Data_Show.showString)(v.value0) + (" " + (Data_Show.show(Data_Show.showString)(v.value1) + ")")));
        };
        throw new Error("Failed pattern match at Affjax.RequestHeader (line 16, column 1 - line 19, column 81): " + [ v.constructor.name ]);
    }
};
var name = function (v) {
    if (v instanceof Accept) {
        return "Accept";
    };
    if (v instanceof ContentType) {
        return "Content-Type";
    };
    if (v instanceof RequestHeader) {
        return v.value0;
    };
    throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [ v.constructor.name ]);
};
var eqRequestHeader = {
    eq: function (x) {
        return function (y) {
            if (x instanceof Accept && y instanceof Accept) {
                return Data_Eq.eq(Data_MediaType.eqMediaType)(x.value0)(y.value0);
            };
            if (x instanceof ContentType && y instanceof ContentType) {
                return Data_Eq.eq(Data_MediaType.eqMediaType)(x.value0)(y.value0);
            };
            if (x instanceof RequestHeader && y instanceof RequestHeader) {
                return x.value0 === y.value0 && x.value1 === y.value1;
            };
            return false;
        };
    }
};
var ordRequestHeader = {
    compare: function (x) {
        return function (y) {
            if (x instanceof Accept && y instanceof Accept) {
                return Data_Ord.compare(Data_MediaType.ordMediaType)(x.value0)(y.value0);
            };
            if (x instanceof Accept) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof Accept) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof ContentType && y instanceof ContentType) {
                return Data_Ord.compare(Data_MediaType.ordMediaType)(x.value0)(y.value0);
            };
            if (x instanceof ContentType) {
                return Data_Ordering.LT.value;
            };
            if (y instanceof ContentType) {
                return Data_Ordering.GT.value;
            };
            if (x instanceof RequestHeader && y instanceof RequestHeader) {
                var v = Data_Ord.compare(Data_Ord.ordString)(x.value0)(y.value0);
                if (v instanceof Data_Ordering.LT) {
                    return Data_Ordering.LT.value;
                };
                if (v instanceof Data_Ordering.GT) {
                    return Data_Ordering.GT.value;
                };
                return Data_Ord.compare(Data_Ord.ordString)(x.value1)(y.value1);
            };
            throw new Error("Failed pattern match at Affjax.RequestHeader (line 14, column 1 - line 14, column 54): " + [ x.constructor.name, y.constructor.name ]);
        };
    },
    Eq0: function () {
        return eqRequestHeader;
    }
};
module.exports = {
    Accept: Accept,
    ContentType: ContentType,
    RequestHeader: RequestHeader,
    name: name,
    value: value,
    eqRequestHeader: eqRequestHeader,
    ordRequestHeader: ordRequestHeader,
    showRequestHeader: showRequestHeader
};
