// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Control_Monad_Logger_Class = require("../Control.Monad.Logger.Class/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var logWarn$prime = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.warn(dictMonadLogger)(Data_Map_Internal.empty);
};
var logWarn = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.warn(dictMonadLogger);
};
var logTrace$prime = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.trace(dictMonadLogger)(Data_Map_Internal.empty);
};
var logTrace = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.trace(dictMonadLogger);
};
var logInfo$prime = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.info(dictMonadLogger)(Data_Map_Internal.empty);
};
var logInfo = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.info(dictMonadLogger);
};
var logError$prime = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.error(dictMonadLogger)(Data_Map_Internal.empty);
};
var logError = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.error(dictMonadLogger);
};
var logDebug$prime = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.debug(dictMonadLogger)(Data_Map_Internal.empty);
};
var logDebug = function (dictMonadLogger) {
    return Control_Monad_Logger_Class.debug(dictMonadLogger);
};
var logAeson = function (dictMonadLogger) {
    return function (dictEncodeAeson) {
        return function (logger) {
            var $22 = Aeson.encodeAeson(dictEncodeAeson);
            return function ($23) {
                return logger(Aeson.stringifyAeson($22($23)));
            };
        };
    };
};
var logAesonDebug = function (dictMonadLogger) {
    return function (dictEncodeAeson) {
        return logAeson(dictMonadLogger)(dictEncodeAeson)(logDebug$prime(dictMonadLogger));
    };
};
var logAesonError = function (dictMonadLogger) {
    return function (dictEncodeAeson) {
        return logAeson(dictMonadLogger)(dictEncodeAeson)(logError$prime(dictMonadLogger));
    };
};
var logAesonInfo = function (dictMonadLogger) {
    return function (dictEncodeAeson) {
        return logAeson(dictMonadLogger)(dictEncodeAeson)(logInfo$prime(dictMonadLogger));
    };
};
var logAesonTrace = function (dictMonadLogger) {
    return function (dictEncodeAeson) {
        return logAeson(dictMonadLogger)(dictEncodeAeson)(logTrace$prime(dictMonadLogger));
    };
};
var logAesonWarn = function (dictMonadLogger) {
    return function (dictEncodeAeson) {
        return logAeson(dictMonadLogger)(dictEncodeAeson)(logWarn$prime(dictMonadLogger));
    };
};
module.exports = {
    logTrace: logTrace,
    "logTrace'": logTrace$prime,
    logDebug: logDebug,
    "logDebug'": logDebug$prime,
    logInfo: logInfo,
    "logInfo'": logInfo$prime,
    logWarn: logWarn,
    "logWarn'": logWarn$prime,
    logError: logError,
    "logError'": logError$prime,
    logAeson: logAeson,
    logAesonTrace: logAesonTrace,
    logAesonDebug: logAesonDebug,
    logAesonInfo: logAesonInfo,
    logAesonWarn: logAesonWarn,
    logAesonError: logAesonError
};