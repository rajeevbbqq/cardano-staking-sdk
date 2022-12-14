// Generated by purs version 0.14.5
"use strict";
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Map = require("../Data.Map/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var StringTag = (function () {
    function StringTag(value0) {
        this.value0 = value0;
    };
    StringTag.create = function (value0) {
        return new StringTag(value0);
    };
    return StringTag;
})();
var NumberTag = (function () {
    function NumberTag(value0) {
        this.value0 = value0;
    };
    NumberTag.create = function (value0) {
        return new NumberTag(value0);
    };
    return NumberTag;
})();
var IntTag = (function () {
    function IntTag(value0) {
        this.value0 = value0;
    };
    IntTag.create = function (value0) {
        return new IntTag(value0);
    };
    return IntTag;
})();
var BooleanTag = (function () {
    function BooleanTag(value0) {
        this.value0 = value0;
    };
    BooleanTag.create = function (value0) {
        return new BooleanTag(value0);
    };
    return BooleanTag;
})();
var JSDateTag = (function () {
    function JSDateTag(value0) {
        this.value0 = value0;
    };
    JSDateTag.create = function (value0) {
        return new JSDateTag(value0);
    };
    return JSDateTag;
})();
var TagSetTag = (function () {
    function TagSetTag(value0) {
        this.value0 = value0;
    };
    TagSetTag.create = function (value0) {
        return new TagSetTag(value0);
    };
    return TagSetTag;
})();
var mkTagType = function (tagger) {
    return function (name) {
        var $0 = Data_Map_Internal.singleton(name);
        return function ($1) {
            return $0(tagger($1));
        };
    };
};
var numberTag = mkTagType(NumberTag.create);
var tag = mkTagType(StringTag.create);
var tagSetTag = mkTagType(TagSetTag.create);
var jsDateTag = mkTagType(JSDateTag.create);
var intTag = mkTagType(IntTag.create);
var fromArray = Data_Map_Internal.unions(Data_Ord.ordString)(Data_Foldable.foldableArray);
var booleanTag = mkTagType(BooleanTag.create);
module.exports = {
    StringTag: StringTag,
    NumberTag: NumberTag,
    IntTag: IntTag,
    BooleanTag: BooleanTag,
    JSDateTag: JSDateTag,
    TagSetTag: TagSetTag,
    tag: tag,
    intTag: intTag,
    numberTag: numberTag,
    booleanTag: booleanTag,
    jsDateTag: jsDateTag,
    tagSetTag: tagSetTag,
    fromArray: fromArray,
    empty: Data_Map.empty,
    singleton: Data_Map.singleton,
    unions: Data_Map.unions
};
