// Generated by purs version 0.14.5
"use strict";
var Data_Unit = require("../Data.Unit/index.js");
var QueryM = require("../QueryM/index.js");
var QueryM_Ogmios = require("../QueryM.Ogmios/index.js");
var getEraSummaries = QueryM.mkOgmiosRequest(QueryM_Ogmios.queryEraSummariesCall)(function (v) {
    return v.eraSummaries;
})(Data_Unit.unit);
module.exports = {
    getEraSummaries: getEraSummaries
};
