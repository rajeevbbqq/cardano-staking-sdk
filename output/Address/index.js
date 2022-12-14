// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var Types_Scripts = require("../Types.Scripts/index.js");
var ogmiosAddressToAddress = Serialization_Address.addressFromBech32;
var enterpriseAddressScriptHash = Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)(Serialization_Address.stakeCredentialToScriptHash)(Control_Bind.composeKleisliFlipped(Data_Maybe.bindMaybe)((function () {
    var $0 = Control_Applicative.pure(Data_Maybe.applicativeMaybe);
    return function ($1) {
        return $0(Serialization_Address.enterpriseAddressPaymentCred($1));
    };
})())(Serialization_Address.enterpriseAddressFromAddress));
var enterpriseAddressStakeValidatorHash = (function () {
    var $2 = Data_Functor.map(Data_Maybe.functorMaybe)(Types_Scripts.StakeValidatorHash);
    return function ($3) {
        return $2(enterpriseAddressScriptHash($3));
    };
})();
var enterpriseAddressValidatorHash = (function () {
    var $4 = Data_Functor.map(Data_Maybe.functorMaybe)(Types_Scripts.ValidatorHash);
    return function ($5) {
        return $4(enterpriseAddressScriptHash($5));
    };
})();
var enterpriseAddressMintingPolicyHash = (function () {
    var $6 = Data_Functor.map(Data_Maybe.functorMaybe)(Types_Scripts.MintingPolicyHash);
    return function ($7) {
        return $6(enterpriseAddressScriptHash($7));
    };
})();
var addressToOgmiosAddress = Serialization_Address.addressBech32;
module.exports = {
    addressToOgmiosAddress: addressToOgmiosAddress,
    enterpriseAddressMintingPolicyHash: enterpriseAddressMintingPolicyHash,
    enterpriseAddressScriptHash: enterpriseAddressScriptHash,
    enterpriseAddressStakeValidatorHash: enterpriseAddressStakeValidatorHash,
    enterpriseAddressValidatorHash: enterpriseAddressValidatorHash,
    ogmiosAddressToAddress: ogmiosAddressToAddress
};
