// Generated by purs version 0.14.5
"use strict";
var Data_Lens_Iso = require("../Data.Lens.Iso/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var _Newtype = function (dictNewtype) {
    return function (dictNewtype1) {
        return function (dictProfunctor) {
            return Data_Lens_Iso.iso(Data_Newtype.unwrap())(Data_Newtype.wrap())(dictProfunctor);
        };
    };
};
var unto = function (dictNewtype) {
    return function (v) {
        return function (dictProfunctor) {
            return _Newtype()()(dictProfunctor);
        };
    };
};
module.exports = {
    "_Newtype": _Newtype,
    unto: unto
};