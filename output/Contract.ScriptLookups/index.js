// Generated by purs version 0.14.5
"use strict";
var Contract_Monad = require("../Contract.Monad/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Types_ScriptLookups = require("../Types.ScriptLookups/index.js");
var mkUnbalancedTx = function (dictValidatorTypes) {
    return function (dictIsData) {
        return function (dictIsData1) {
            return function (lookups) {
                var $6 = Types_ScriptLookups.mkUnbalancedTx()(dictIsData)(dictIsData1)(lookups);
                return function ($7) {
                    return Contract_Monad.wrapContract($6($7));
                };
            };
        };
    };
};
var mkUnbalancedTxM = function (dictValidatorTypes) {
    return function (dictIsData) {
        return function (dictIsData1) {
            return function (lookups) {
                var $8 = Data_Functor.map(Contract_Monad.functorContract)(Data_Either.hush);
                var $9 = mkUnbalancedTx()(dictIsData)(dictIsData1)(lookups);
                return function ($10) {
                    return $8($9($10));
                };
            };
        };
    };
};
module.exports = {
    mkUnbalancedTx: mkUnbalancedTx,
    mkUnbalancedTxM: mkUnbalancedTxM,
    CannotConvertPOSIXTimeRange: Types_ScriptLookups.CannotConvertPOSIXTimeRange,
    CannotConvertPaymentPubKeyHash: Types_ScriptLookups.CannotConvertPaymentPubKeyHash,
    CannotGetMintingPolicyScriptIndex: Types_ScriptLookups.CannotGetMintingPolicyScriptIndex,
    CannotGetValidatorHashFromAddress: Types_ScriptLookups.CannotGetValidatorHashFromAddress,
    CannotHashDatum: Types_ScriptLookups.CannotHashDatum,
    CannotHashMintingPolicy: Types_ScriptLookups.CannotHashMintingPolicy,
    CannotHashValidator: Types_ScriptLookups.CannotHashValidator,
    CannotMakeValue: Types_ScriptLookups.CannotMakeValue,
    CannotQueryDatum: Types_ScriptLookups.CannotQueryDatum,
    CannotSatisfyAny: Types_ScriptLookups.CannotSatisfyAny,
    DatumNotFound: Types_ScriptLookups.DatumNotFound,
    DatumWrongHash: Types_ScriptLookups.DatumWrongHash,
    MintingPolicyHashNotCurrencySymbol: Types_ScriptLookups.MintingPolicyHashNotCurrencySymbol,
    MintingPolicyNotFound: Types_ScriptLookups.MintingPolicyNotFound,
    MkTypedTxOutFailed: Types_ScriptLookups.MkTypedTxOutFailed,
    ModifyTx: Types_ScriptLookups.ModifyTx,
    OwnPubKeyAndStakeKeyMissing: Types_ScriptLookups.OwnPubKeyAndStakeKeyMissing,
    TxOutRefNotFound: Types_ScriptLookups.TxOutRefNotFound,
    TxOutRefWrongType: Types_ScriptLookups.TxOutRefWrongType,
    TypeCheckFailed: Types_ScriptLookups.TypeCheckFailed,
    TypedTxOutHasNoDatumHash: Types_ScriptLookups.TypedTxOutHasNoDatumHash,
    TypedValidatorMissing: Types_ScriptLookups.TypedValidatorMissing,
    ValidatorHashNotFound: Types_ScriptLookups.ValidatorHashNotFound,
    ScriptLookups: Types_ScriptLookups.ScriptLookups,
    UnattachedUnbalancedTx: Types_ScriptLookups.UnattachedUnbalancedTx,
    datum: Types_ScriptLookups.datum,
    generalise: Types_ScriptLookups.generalise,
    mintingPolicy: Types_ScriptLookups.mintingPolicy,
    mintingPolicyM: Types_ScriptLookups.mintingPolicyM,
    ownPaymentPubKeyHash: Types_ScriptLookups.ownPaymentPubKeyHash,
    ownPaymentPubKeyHashM: Types_ScriptLookups.ownPaymentPubKeyHashM,
    ownStakePubKeyHash: Types_ScriptLookups.ownStakePubKeyHash,
    ownStakePubKeyHashM: Types_ScriptLookups.ownStakePubKeyHashM,
    typedValidatorLookups: Types_ScriptLookups.typedValidatorLookups,
    typedValidatorLookupsM: Types_ScriptLookups.typedValidatorLookupsM,
    unspentOutputs: Types_ScriptLookups.unspentOutputs,
    unspentOutputsM: Types_ScriptLookups.unspentOutputsM,
    validator: Types_ScriptLookups.validator,
    validatorM: Types_ScriptLookups.validatorM
};
