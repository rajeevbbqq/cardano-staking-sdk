// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Control_Alt = require("../Control.Alt/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Argonaut_Decode_Error = require("../Data.Argonaut.Decode.Error/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Array_NonEmpty = require("../Data.Array.NonEmpty/index.js");
var Data_Array_NonEmpty_Internal = require("../Data.Array.NonEmpty.Internal/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Generic_Rep = require("../Data.Generic.Rep/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ordering = require("../Data.Ordering/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Show_Generic = require("../Data.Show.Generic/index.js");
var Data_TextEncoder = require("../Data.TextEncoder/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Foreign_Object = require("../Foreign.Object/index.js");
var FromData = require("../FromData/index.js");
var Metadata_Cip25_Cip25String = require("../Metadata.Cip25.Cip25String/index.js");
var Metadata_Cip25_Common = require("../Metadata.Cip25.Common/index.js");
var Metadata_FromMetadata = require("../Metadata.FromMetadata/index.js");
var Metadata_Helpers = require("../Metadata.Helpers/index.js");
var Metadata_ToMetadata = require("../Metadata.ToMetadata/index.js");
var Plutus_Types_AssocMap = require("../Plutus.Types.AssocMap/index.js");
var Serialization_Hash = require("../Serialization.Hash/index.js");
var ToData = require("../ToData/index.js");
var Types_Int = require("../Types.Int/index.js");
var Types_PlutusData = require("../Types.PlutusData/index.js");
var Types_RawBytes = require("../Types.RawBytes/index.js");
var Types_Scripts = require("../Types.Scripts/index.js");
var Types_TokenName = require("../Types.TokenName/index.js");
var Types_TransactionMetadata = require("../Types.TransactionMetadata/index.js");
var Cip25V2 = (function () {
    function Cip25V2() {

    };
    Cip25V2.value = new Cip25V2();
    return Cip25V2;
})();
var Cip25MetadataEntry = function (x) {
    return x;
};
var Cip25Metadata = function (x) {
    return x;
};
var toMetadataCip25V2 = {
    toMetadata: function (v) {
        return Types_TransactionMetadata.Int.create(Data_Maybe.fromJust()(Types_Int.fromBigInt(Data_BigInt.fromInt(2))));
    }
};
var toDataCip25V2 = {
    toData: function (v) {
        return Types_PlutusData.Integer.create(Data_BigInt.fromInt(2));
    }
};
var newtypeCip25Metadata_ = {
    Coercible0: function () {
        return undefined;
    }
};
var newtypeCip25MetadataEntry = {
    Coercible0: function () {
        return undefined;
    }
};
var genericCip25V2_ = {
    to: function (x) {
        return Cip25V2.value;
    },
    from: function (x) {
        return Data_Generic_Rep.NoArguments.value;
    }
};
var showCip25V2 = {
    show: Data_Show_Generic.genericShow(genericCip25V2_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsNoArguments)({
        reflectSymbol: function () {
            return "Cip25V2";
        }
    }))
};
var genericCip25Metadata_ = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var genericCip25MetadataEntry = {
    to: function (x) {
        return x;
    },
    from: function (x) {
        return x;
    }
};
var showCip25MetadataEntry = {
    show: Data_Show_Generic.genericShow(genericCip25MetadataEntry)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showRecord()(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "assetName";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "description";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "files";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "image";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "mediaType";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "name";
        }
    })(Data_Show.showRecordFieldsCons({
        reflectSymbol: function () {
            return "policyId";
        }
    })(Data_Show.showRecordFieldsNil)(Types_Scripts.showMintingPolicyHash))(Metadata_Cip25_Cip25String.showCip25String))(Data_Maybe.showMaybe(Metadata_Cip25_Cip25String.showCip25String)))(Data_Show.showString))(Data_Show.showArray(Metadata_Cip25_Common.showCip25MetadataFile)))(Data_Maybe.showMaybe(Data_Show.showString)))(Metadata_Cip25_Common.showCip25TokenName))))({
        reflectSymbol: function () {
            return "Cip25MetadataEntry";
        }
    }))
};
var showCip25Metadata = {
    show: Data_Show_Generic.genericShow(genericCip25Metadata_)(Data_Show_Generic.genericShowConstructor(Data_Show_Generic.genericShowArgsArgument(Data_Show.showArray(showCip25MetadataEntry)))({
        reflectSymbol: function () {
            return "Cip25Metadata";
        }
    }))
};
var fromMetadataCip25V2 = {
    fromMetadata: function (v) {
        if (v instanceof Types_TransactionMetadata.Int && Data_Eq.eq(Data_BigInt.eqBigInt)(Types_Int.toBigInt(v.value0))(Data_BigInt.fromInt(2))) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Cip25V2.value);
        };
        return Data_Maybe.Nothing.value;
    }
};
var fromDataCip25V2 = {
    fromData: function (v) {
        if (v instanceof Types_PlutusData.Integer && Data_Eq.eq(Data_BigInt.eqBigInt)(v.value0)(Data_BigInt.fromInt(2))) {
            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Cip25V2.value);
        };
        return Data_Maybe.Nothing.value;
    }
};
var eqCip25V2 = {
    eq: function (x) {
        return function (y) {
            return true;
        };
    }
};
var ordCip25V2 = {
    compare: function (x) {
        return function (y) {
            return Data_Ordering.EQ.value;
        };
    },
    Eq0: function () {
        return eqCip25V2;
    }
};
var eqCip25MetadataEntry = {
    eq: function (x) {
        return function (y) {
            return Data_Eq.eq(Metadata_Cip25_Common.eqCip25TokenName)(x.assetName)(y.assetName) && Data_Eq.eq(Data_Maybe.eqMaybe(Data_Eq.eqString))(x.description)(y.description) && Data_Eq.eq(Data_Eq.eqArray(Metadata_Cip25_Common.eqCip25MetadataFile))(x.files)(y.files) && x.image === y.image && Data_Eq.eq(Data_Maybe.eqMaybe(Metadata_Cip25_Cip25String.eqCip25String))(x.mediaType)(y.mediaType) && Data_Eq.eq(Metadata_Cip25_Cip25String.eqCip25String)(x.name)(y.name) && Data_Eq.eq(Types_Scripts.eqMintingPolicyHash)(x.policyId)(y.policyId);
        };
    }
};
var eqCip25Metadata = {
    eq: function (x) {
        return function (y) {
            return Data_Eq.eq(Data_Eq.eqArray(eqCip25MetadataEntry))(x)(y);
        };
    }
};
var decodeAesonCip25V2 = {
    decodeAeson: function (aeson) {
        return Control_Bind.bind(Data_Either.bindEither)(Aeson.decodeAeson(Aeson.decodeAesonInt)(aeson))(function (v) {
            if (v === 2) {
                return Control_Applicative.pure(Data_Either.applicativeEither)(Cip25V2.value);
            };
            return Data_Either.Left.create(new Data_Argonaut_Decode_Error.TypeMismatch("Cip25V2"));
        });
    }
};
var metadataEntryToMetadata = function (v) {
    var mbMediaType = Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray)(Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(v.mediaType)(function (mediaType) {
        return [ new Data_Tuple.Tuple("mediaType", Metadata_ToMetadata.anyToMetadata(Metadata_ToMetadata.anyToMetadataClass(Metadata_Cip25_Cip25String.toMetadataCip25String))(mediaType)) ];
    }));
    var mbFiles = (function () {
        if (v.files.length === 0) {
            return [  ];
        };
        return [ new Data_Tuple.Tuple("files", Metadata_ToMetadata.anyToMetadata(Metadata_ToMetadata.anyToMetadataClass(Metadata_ToMetadata.toMetadataArray(Metadata_Cip25_Common.toMetadataCip25MetadataFi)))(v.files)) ];
    })();
    var mbDescription = Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray)(Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(v.description)(function (description) {
        return [ new Data_Tuple.Tuple("description", Metadata_ToMetadata.anyToMetadata(Metadata_ToMetadata.anyToMetadataClass(Metadata_ToMetadata.toMetadataString))(description)) ];
    }));
    return Metadata_ToMetadata.toMetadata(Metadata_ToMetadata.toMetadataArrayTupleAnyTo(Data_Ord.ordString)(Metadata_ToMetadata.toMetadataString))(Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple("name", Metadata_ToMetadata.anyToMetadata(Metadata_ToMetadata.anyToMetadataClass(Metadata_Cip25_Cip25String.toMetadataCip25String))(v.name)), new Data_Tuple.Tuple("image", Metadata_ToMetadata.anyToMetadata(Metadata_ToMetadata.anyToMetadataClass(Metadata_ToMetadata.toMetadataString))(v.image)) ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(mbMediaType)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(mbDescription)(mbFiles))));
};
var metadataEntryToData = function (v) {
    var mbMediaType = Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray)(Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(v.mediaType)(function (mediaType) {
        return [ new Data_Tuple.Tuple("mediaType", ToData.toData(Metadata_Cip25_Cip25String.toDataCip25String)(mediaType)) ];
    }));
    var mbFiles = (function () {
        if (v.files.length === 0) {
            return [  ];
        };
        return [ new Data_Tuple.Tuple("files", ToData.toData(ToData.toDataArray(Metadata_Cip25_Common.toDataCip25MetadataFile))(v.files)) ];
    })();
    var mbDescription = Data_Foldable.fold(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray)(Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(v.description)(function (description) {
        return [ new Data_Tuple.Tuple("description", ToData.toData(ToData.toDataString)(description)) ];
    }));
    return ToData.toData(Plutus_Types_AssocMap.toDataMap(ToData.toDataString)(ToData.toDataPlutusData))(Plutus_Types_AssocMap["Map"](Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple("name", ToData.toData(Metadata_Cip25_Cip25String.toDataCip25String)(v.name)), new Data_Tuple.Tuple("image", ToData.toData(ToData.toDataString)(v.image)) ])(Data_Semigroup.append(Data_Semigroup.semigroupArray)(mbMediaType)(Data_Semigroup.append(Data_Semigroup.semigroupArray)(mbDescription)(mbFiles)))));
};
var metadataEntryFromMetadata = function (policyId) {
    return function (assetName) {
        return function (contents) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Control_Bind.bind(Data_Maybe.bindMaybe)(Metadata_Helpers.lookupMetadata("name")(contents))(Metadata_FromMetadata.fromMetadata(Metadata_Cip25_Cip25String.fromMetadataCip25String)))(function (name) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(Control_Bind.bind(Data_Maybe.bindMaybe)(Metadata_Helpers.lookupMetadata("image")(contents))(Metadata_Cip25_Cip25String.fromMetadataString))(function (image) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableMaybe)(Metadata_Helpers.lookupMetadata("mediaType")(contents))(Metadata_FromMetadata.fromMetadata(Metadata_Cip25_Cip25String.fromMetadataCip25String)))(function (mediaType) {
                        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableMaybe)(Metadata_Helpers.lookupMetadata("description")(contents))(Metadata_Cip25_Cip25String.fromMetadataString))(function (description) {
                            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableMaybe)(Metadata_Helpers.lookupMetadata("files")(contents))(Metadata_FromMetadata.fromMetadata(Metadata_FromMetadata.fromMetadataArray(Metadata_Cip25_Common.fromMetadataCip25Metadata))))(Data_Maybe.fromMaybe([  ])))(function (files) {
                                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Newtype.wrap()({
                                    policyId: policyId,
                                    assetName: assetName,
                                    name: name,
                                    image: image,
                                    mediaType: mediaType,
                                    description: description,
                                    files: files
                                }));
                            });
                        });
                    });
                });
            });
        };
    };
};
var fromMetadataCip25Metadata = {
    fromMetadata: function (v) {
        if (v instanceof Types_TransactionMetadata.MetadataMap) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Array.concat)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Array.catMaybes)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(v.value0))(function (v1) {
                var $88 = Data_Eq.eq(Types_TransactionMetadata.eqTransactionMetadatum)(v1.value0)(Metadata_ToMetadata.toMetadata(Metadata_ToMetadata.toMetadataString)("version"));
                if ($88) {
                    var $89 = Data_Eq.eq(Types_TransactionMetadata.eqTransactionMetadatum)(v1.value1)(Metadata_ToMetadata.toMetadata(toMetadataCip25V2)(Cip25V2.value));
                    if ($89) {
                        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Maybe.Nothing.value);
                    };
                    return Data_Maybe.Nothing.value;
                };
                return new Data_Maybe.Just((function () {
                    if (v1.value1 instanceof Types_TransactionMetadata.MetadataMap) {
                        return Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(v1.value1.value0))(function (v2) {
                            return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(metadataEntryFromMetadata)(Metadata_FromMetadata.fromMetadata(Types_Scripts.fromMetadataMintingPolicy)(v1.value0)))(Metadata_FromMetadata.fromMetadata(Metadata_Cip25_Common.fromMetadataCip25TokenNam)(v2.value0)))(Control_Applicative.pure(Data_Maybe.applicativeMaybe)(v2.value1));
                        });
                    };
                    return Data_Maybe.Nothing.value;
                })());
            }))))(function (entries) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap())(Data_Traversable.sequence(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(entries));
            });
        };
        return Data_Maybe.Nothing.value;
    }
};
var metadataEntryFromData = function (policyId) {
    return function (assetName) {
        return function (contents) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Control_Bind.bind(Data_Maybe.bindMaybe)(Metadata_Helpers.lookupKey("name")(contents))(FromData.fromData(Metadata_Cip25_Cip25String.fromDataCip25String)))(function (name) {
                return Control_Bind.bind(Data_Maybe.bindMaybe)(Control_Bind.bind(Data_Maybe.bindMaybe)(Metadata_Helpers.lookupKey("image")(contents))(Metadata_Cip25_Cip25String.fromDataString))(function (image) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableMaybe)(Metadata_Helpers.lookupKey("mediaType")(contents))(FromData.fromData(Metadata_Cip25_Cip25String.fromDataCip25String)))(function (mediaType) {
                        return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableMaybe)(Metadata_Helpers.lookupKey("description")(contents))(Metadata_Cip25_Cip25String.fromDataString))(function (description) {
                            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableMaybe)(Metadata_Helpers.lookupKey("files")(contents))(FromData.fromData(FromData.fromDataArray(Metadata_Cip25_Common.fromDataCip25MetadataFile))))(Data_Maybe.fromMaybe([  ])))(function (files) {
                                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Newtype.wrap()({
                                    policyId: policyId,
                                    assetName: assetName,
                                    name: name,
                                    image: image,
                                    mediaType: mediaType,
                                    description: description,
                                    files: files
                                }));
                            });
                        });
                    });
                });
            });
        };
    };
};
var fromDataCip25Metadata = {
    fromData: function (meta) {
        return Control_Bind.bind(Data_Maybe.bindMaybe)(Control_Bind.bind(Data_Maybe.bindMaybe)(Metadata_Helpers.lookupKey(Data_BigInt.toString(Metadata_Cip25_Common.nftMetadataLabel))(meta))(function (v) {
            if (v instanceof Types_PlutusData["Map"]) {
                return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Array.concat)(Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(v.value0)(function (v1) {
                    var fromDataVersion = (function () {
                        var $100 = Data_Eq.eq(Types_PlutusData.eqPlutusData)(v1.value0)(ToData.toData(ToData.toDataString)("version")) && Data_Eq.eq(Types_PlutusData.eqPlutusData)(v1.value1)(ToData.toData(toDataCip25V2)(Cip25V2.value));
                        if ($100) {
                            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)([  ]);
                        };
                        return Data_Maybe.Nothing.value;
                    })();
                    var fromDataAssets = (function () {
                        if (v1.value1 instanceof Types_PlutusData["Map"]) {
                            return Data_Traversable["for"](Data_Maybe.applicativeMaybe)(Data_Traversable.traversableArray)(v1.value1.value0)(function (v2) {
                                return Control_Apply.apply(Data_Maybe.applyMaybe)(Control_Apply.apply(Data_Maybe.applyMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(metadataEntryFromData)(FromData.fromData(Types_Scripts.fromDataMintingPolicyHash)(v1.value0)))(FromData.fromData(Metadata_Cip25_Common.fromDataCip25TokenName)(v2.value0)))(Control_Applicative.pure(Data_Maybe.applicativeMaybe)(v2.value1));
                            });
                        };
                        return Data_Maybe.Nothing.value;
                    })();
                    return Control_Alt.alt(Data_Maybe.altMaybe)(fromDataAssets)(fromDataVersion);
                }));
            };
            return Data_Maybe.Nothing.value;
        }))(function (entries) {
            return Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap())(Data_Traversable.sequence(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(entries));
        });
    }
};
var metadataEntryDecodeAeson = function (policyId) {
    return function (assetName) {
        return Aeson.caseAesonObject(Metadata_Helpers.errExpectedObject)(function (obj) {
            return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Metadata_Cip25_Cip25String.decodeAesonCip25String)(obj)("name"))(function (name) {
                return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonString)(obj)("image"))(function (image) {
                    return Control_Bind.bind(Data_Either.bindEither)(Aeson["getFieldOptional'"](Metadata_Cip25_Cip25String.decodeAesonCip25String)(obj)("mediaType"))(function (mediaType) {
                        return Control_Bind.bind(Data_Either.bindEither)(Data_Functor.mapFlipped(Data_Either.functorEither)(Aeson["getFieldOptional'"](Aeson.decodeAesonMaybe(Aeson.decodeAesonString))(obj)("description"))(Data_Maybe.fromMaybe(Data_Monoid.mempty(Data_Maybe.monoidMaybe(Data_Semigroup.semigroupString)))))(function (description) {
                            return Control_Bind.bind(Data_Either.bindEither)(Control_Bind.bind(Data_Either.bindEither)(Aeson["getFieldOptional'"](Aeson.decodeAesonAeson)(obj)("files"))(function (mbFiles) {
                                return Data_Functor.map(Data_Either.functorEither)(Data_Maybe.fromMaybe([  ]))(Data_Traversable["for"](Data_Either.applicativeEither)(Data_Traversable.traversableMaybe)(mbFiles)(function (files) {
                                    return Control_Bind.bindFlipped(Data_Either.bindEither)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Either.applicativeEither)(Aeson.decodeAeson(Metadata_Cip25_Common.decodeAesonCip25MetadataF)))(Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("files"))(Aeson.toArray(files)));
                                }));
                            }))(function (files) {
                                return Control_Applicative.pure(Data_Either.applicativeEither)(Data_Newtype.wrap()({
                                    policyId: policyId,
                                    assetName: assetName,
                                    name: name,
                                    image: image,
                                    mediaType: mediaType,
                                    description: description,
                                    files: files
                                }));
                            });
                        });
                    });
                });
            });
        });
    };
};
var decodeAesonCip25Metadata = {
    decodeAeson: (function () {
        var withJsonObject = Data_Function.flip(Aeson.caseAesonObject(Metadata_Helpers.errExpectedObject));
        var objToArray = Foreign_Object.toUnfoldable(Data_Unfoldable.unfoldableArray);
        var decodePolicyId = (function () {
            var $117 = Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("Expected hex-encoded policy id"));
            var $118 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap());
            var $119 = Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(Serialization_Hash.scriptHashFromBytes)(Types_RawBytes.hexToRawBytes);
            return function ($120) {
                return $117($118($119($120)));
            };
        })();
        var decodeAssetName = (function () {
            var $121 = Data_Either.note(new Data_Argonaut_Decode_Error.TypeMismatch("Expected UTF-8 encoded asset name"));
            var $122 = Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap());
            var $123 = Data_Newtype.wrap();
            return function ($124) {
                return $121($122(Types_TokenName.mkTokenName($123(Data_TextEncoder.encodeUtf8($124)))));
            };
        })();
        return Aeson.caseAesonObject(Metadata_Helpers.errExpectedObject)(function (obj) {
            return Control_Bind.bind(Data_Either.bindEither)(Aeson.getField(Aeson.decodeAesonAeson)(obj)(Data_BigInt.toString(Metadata_Cip25_Common.nftMetadataLabel)))(function (policies) {
                return withJsonObject(policies)(function (objPolicies) {
                    return Data_Functor.map(Data_Either.functorEither)((function () {
                        var $125 = Data_Newtype.wrap();
                        return function ($126) {
                            return $125(Data_Array.concat($126));
                        };
                    })())(Data_Traversable["for"](Data_Either.applicativeEither)(Data_Traversable.traversableArray)(objToArray(objPolicies))(function (v) {
                        return withJsonObject(v.value1)(function (objAssets) {
                            return Data_Traversable["for"](Data_Either.applicativeEither)(Data_Traversable.traversableArray)(objToArray(objAssets))(function (v1) {
                                return Control_Bind.bind(Data_Either.bindEither)(decodePolicyId(v.value0))(function (policyId_) {
                                    return Control_Bind.bind(Data_Either.bindEither)(decodeAssetName(v1.value0))(function (assetName_) {
                                        return metadataEntryDecodeAeson(policyId_)(assetName_)(v1.value1);
                                    });
                                });
                            });
                        });
                    }));
                });
            });
        });
    })()
};
var groupEntries = Data_Array.groupBy(Data_Function.on(Data_Eq.eq(Types_Scripts.eqMintingPolicyHash))((function () {
    var $127 = Data_Newtype.unwrap();
    return function ($128) {
        return (function (v) {
            return v.policyId;
        })($127($128));
    };
})()));
var toDataCip25Metadata = {
    toData: function (v) {
        return ToData.toData(Plutus_Types_AssocMap.toDataMap(ToData.toDataPlutusData)(Plutus_Types_AssocMap.toDataMap(ToData.toDataPlutusData)(ToData.toDataPlutusData)))(Plutus_Types_AssocMap.singleton(ToData.toData(ToData.toDataString)(Data_BigInt.toString(Metadata_Cip25_Common.nftMetadataLabel)))((function () {
            var versionEntry = [ new Data_Tuple.Tuple(ToData.toData(ToData.toDataString)("version"), ToData.toData(toDataCip25V2)(Cip25V2.value)) ];
            var dataEntries = Data_Functor.mapFlipped(Data_Functor.functorArray)(groupEntries(v))(function (group) {
                return new Data_Tuple.Tuple(ToData.toData(Types_Scripts.toDataMintingPolicyHash)((function (v1) {
                    return v1.policyId;
                })(Data_Newtype.unwrap()(Data_Array_NonEmpty.head(group)))), ToData.toData(Plutus_Types_AssocMap.toDataMap(ToData.toDataPlutusData)(ToData.toDataPlutusData))(Plutus_Types_AssocMap["Map"](Data_Array_NonEmpty.toArray(Data_Function.flip(Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray))(group)(function (entry) {
                    return new Data_Tuple.Tuple(ToData.toData(Metadata_Cip25_Common.toDataCip25TokenName)((Data_Newtype.unwrap()(entry)).assetName), metadataEntryToData(entry));
                })))));
            });
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(dataEntries)(versionEntry);
        })()));
    }
};
var toMetadataCip25Metadata = {
    toMetadata: function (v) {
        return Metadata_ToMetadata.toMetadata(Metadata_ToMetadata.toMetadataArrayTuple(Types_TransactionMetadata.ordTransactionMetadatum)(Metadata_ToMetadata.toMetadataTransactionMeta)(Metadata_ToMetadata.toMetadataTransactionMeta))((function () {
            var versionEntry = [ new Data_Tuple.Tuple(Metadata_ToMetadata.toMetadata(Metadata_ToMetadata.toMetadataString)("version"), Metadata_ToMetadata.toMetadata(toMetadataCip25V2)(Cip25V2.value)) ];
            var dataEntries = Data_Functor.mapFlipped(Data_Functor.functorArray)(groupEntries(v))(function (group) {
                return new Data_Tuple.Tuple(Metadata_ToMetadata.toMetadata(Types_Scripts.toMetadataMintingPolicyHa)((function (v1) {
                    return v1.policyId;
                })(Data_Newtype.unwrap()(Data_Array_NonEmpty.head(group)))), Metadata_ToMetadata.toMetadata(Metadata_ToMetadata.toMetadataArrayTuple(Metadata_Cip25_Common.ordCip25TokenName)(Metadata_Cip25_Common.toMetadataCip25TokenName)(Metadata_ToMetadata.toMetadataTransactionMeta))(Data_Array_NonEmpty.toArray(Data_Function.flip(Data_Functor.map(Data_Array_NonEmpty_Internal.functorNonEmptyArray))(group)(function (entry) {
                    return new Data_Tuple.Tuple((Data_Newtype.unwrap()(entry)).assetName, metadataEntryToMetadata(entry));
                }))));
            });
            return Data_Semigroup.append(Data_Semigroup.semigroupArray)(dataEntries)(versionEntry);
        })());
    }
};
var metadataTypeCip25Metadata = {
    metadataLabel: function (v) {
        return Data_Newtype.wrap()(Metadata_Cip25_Common.nftMetadataLabel);
    },
    FromMetadata0: function () {
        return fromMetadataCip25Metadata;
    },
    ToMetadata1: function () {
        return toMetadataCip25Metadata;
    }
};
module.exports = {
    Cip25Metadata: Cip25Metadata,
    Cip25MetadataEntry: Cip25MetadataEntry,
    genericCip25MetadataEntry: genericCip25MetadataEntry,
    newtypeCip25MetadataEntry: newtypeCip25MetadataEntry,
    eqCip25MetadataEntry: eqCip25MetadataEntry,
    showCip25MetadataEntry: showCip25MetadataEntry,
    genericCip25Metadata_: genericCip25Metadata_,
    newtypeCip25Metadata_: newtypeCip25Metadata_,
    eqCip25Metadata: eqCip25Metadata,
    showCip25Metadata: showCip25Metadata,
    metadataTypeCip25Metadata: metadataTypeCip25Metadata,
    toMetadataCip25Metadata: toMetadataCip25Metadata,
    fromMetadataCip25Metadata: fromMetadataCip25Metadata,
    toDataCip25Metadata: toDataCip25Metadata,
    fromDataCip25Metadata: fromDataCip25Metadata,
    decodeAesonCip25Metadata: decodeAesonCip25Metadata,
    Cip25MetadataFile: Metadata_Cip25_Common.Cip25MetadataFile,
    Cip25TokenName: Metadata_Cip25_Common.Cip25TokenName,
    nftMetadataLabel: Metadata_Cip25_Common.nftMetadataLabel
};
