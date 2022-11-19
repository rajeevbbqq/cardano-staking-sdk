// Generated by purs version 0.14.5
"use strict";
var Control_Category = require("../Control.Category/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Profunctor = require("../Data.Profunctor/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Record = require("../Record/index.js");
var unconsRecord = function (dictIsSymbol) {
    return function (dictLacks) {
        return function (dictCons) {
            return function (p) {
                return function (record) {
                    return new Data_Tuple.Tuple(Record.get(dictIsSymbol)()(p)(record), Record["delete"](dictIsSymbol)()()(p)(record));
                };
            };
        };
    };
};
var tagProp = "tag";
var rightProp = "Right";
var maybeToEither = function (a) {
    return Data_Maybe.maybe(new Data_Either.Left(a))(Data_Either.Right.create);
};
var mapP = function (dictProfunctor) {
    return Data_Profunctor.dimap(dictProfunctor)(Control_Category.identity(Control_Category.categoryFn));
};
var leftProp = "Left";
var contentsProp = "contents";
var cmapP = function (dictProfunctor) {
    return function (f) {
        return Data_Profunctor.dimap(dictProfunctor)(f)(Control_Category.identity(Control_Category.categoryFn));
    };
};
module.exports = {
    leftProp: leftProp,
    rightProp: rightProp,
    tagProp: tagProp,
    contentsProp: contentsProp,
    maybeToEither: maybeToEither,
    unconsRecord: unconsRecord,
    mapP: mapP,
    cmapP: cmapP
};
