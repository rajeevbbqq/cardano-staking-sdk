// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var QueryM_UniqueId = require("../QueryM.UniqueId/index.js");
var Record = require("../Record/index.js");
var JsonWspCall = function (x) {
    return x;
};
var parseMirror = Aeson.caseAesonString(new Data_Either.Left(new Data_Argonaut_Decode_Error.TypeMismatch("expected string")))(Control_Applicative.pure(Data_Either.applicativeEither));
var mkJsonWspRequest = function (service) {
    return function (method) {
        return function __do() {
            var id = QueryM_UniqueId.uniqueId(method.methodname + "-")();
            return Record.merge()()({
                mirror: id
            })(Record.merge()()(service)(method));
        };
    };
};
var mkCallType = function (dictEncodeAeson) {
    return function (service) {
        return function (v) {
            return JsonWspCall(function (i) {
                return function __do() {
                    var req = mkJsonWspRequest(service)({
                        methodname: v.methodname,
                        args: v.args(i)
                    })();
                    return {
                        body: Aeson.encodeAeson(dictEncodeAeson)(req),
                        id: req.mirror
                    };
                };
            });
        };
    };
};
var buildRequest = function (v) {
    return v;
};
var aesonObject = Aeson.caseAesonObject(new Data_Either.Left(new Data_Argonaut_Decode_Error.TypeMismatch("expected object")));
var parseJsonWspResponse = function (dictDecodeAeson) {
    return aesonObject(function (o) {
        return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonString)(o)("type"))(function (typeField) {
            return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonString)(o)("version"))(function (version) {
                return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonString)(o)("servicename"))(function (servicename) {
                    return Control_Bind.bind(Data_Either.bindEither)(Aeson.getFieldOptional(Aeson.decodeAesonString)(o)("methodname"))(function (methodname) {
                        return Control_Bind.bind(Data_Either.bindEither)(Control_Bind.bindFlipped(Data_Either.bindEither)(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Data_Either.applicativeEither)(Aeson.decodeAeson(dictDecodeAeson)))(Aeson.getFieldOptional(Aeson.decodeAesonAeson)(o)("result")))(function (result) {
                            return Control_Bind.bind(Data_Either.bindEither)(Control_Bind.bindFlipped(Data_Either.bindEither)(Data_Traversable.traverse(Data_Traversable.traversableMaybe)(Data_Either.applicativeEither)(Aeson.decodeAeson(Aeson.decodeAesonAeson)))(Aeson.getFieldOptional(Aeson.decodeAesonAeson)(o)("fault")))(function (fault) {
                                return Control_Bind.bind(Data_Either.bindEither)(Control_Bind.bindFlipped(Data_Either.bindEither)(parseMirror)(Aeson.getField(Aeson.decodeAesonAeson)(o)("reflection")))(function (reflection) {
                                    return Control_Applicative.pure(Data_Either.applicativeEither)({
                                        type: typeField,
                                        version: version,
                                        servicename: servicename,
                                        methodname: methodname,
                                        result: result,
                                        fault: fault,
                                        reflection: reflection
                                    });
                                });
                            });
                        });
                    });
                });
            });
        });
    });
};
var parseJsonWspResponseId = aesonObject(function (o) {
    return Control_Bind.bindFlipped(Data_Either.bindEither)(parseMirror)(Aeson.getField(Aeson.decodeAesonAeson)(o)("reflection"));
});
module.exports = {
    mkCallType: mkCallType,
    buildRequest: buildRequest,
    parseJsonWspResponse: parseJsonWspResponse,
    parseJsonWspResponseId: parseJsonWspResponseId
};
