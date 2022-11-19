// Generated by purs version 0.14.5
"use strict";
var Contract_Address = require("../Contract.Address/index.js");
var Contract_Monad = require("../Contract.Monad/index.js");
var Contract_Utxos = require("../Contract.Utxos/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Data_Lens_Common = require("../Data.Lens.Common/index.js");
var Data_Lens_Iso_Newtype = require("../Data.Lens.Iso.Newtype/index.js");
var Data_Lens_Record = require("../Data.Lens.Record/index.js");
var Data_Lens_Setter = require("../Data.Lens.Setter/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Profunctor = require("../Data.Profunctor/index.js");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong/index.js");
var Serialization = require("../Serialization/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Wallet = require("../Wallet/index.js");
var Wallet_Key = require("../Wallet.Key/index.js");
var Wallet_KeyFile = require("../Wallet.KeyFile/index.js");
var Wallet_Spec = require("../Wallet.Spec/index.js");
var withKeyWallet = function (wallet) {
    return function (action) {
        var _wallet = function (dictStrong) {
            return Data_Lens_Record.prop({
                reflectSymbol: function () {
                    return "wallet";
                }
            })()()(Type_Proxy["Proxy"].value)(dictStrong);
        };
        var _runtime = function (dictStrong) {
            return Data_Lens_Record.prop({
                reflectSymbol: function () {
                    return "runtime";
                }
            })()()(Type_Proxy["Proxy"].value)(dictStrong);
        };
        var setUpdatedWallet = Data_Lens_Setter.set((function () {
            var $2 = Data_Lens_Common.simple(Data_Lens_Iso_Newtype["_Newtype"]()()(Data_Profunctor.profunctorFn));
            var $3 = _runtime(Data_Profunctor_Strong.strongFn);
            var $4 = _wallet(Data_Profunctor_Strong.strongFn);
            return function ($5) {
                return $2($3($4($5)));
            };
        })())(new Data_Maybe.Just(new Wallet.KeyWallet(wallet)));
        return Control_Monad_Reader_Class.local(Contract_Monad.monadReaderContractEnvCon)(setUpdatedWallet)(action);
    };
};
var mkKeyWalletFromPrivateKeys = Wallet.mkKeyWallet;
module.exports = {
    mkKeyWalletFromPrivateKeys: mkKeyWalletFromPrivateKeys,
    withKeyWallet: withKeyWallet,
    getWalletAddress: Contract_Address.getWalletAddress,
    getWalletCollateral: Contract_Address.getWalletCollateral,
    getWalletUtxos: Contract_Utxos.getWalletUtxos,
    privateKeyFromBytes: Serialization.privateKeyFromBytes,
    isEternlAvailable: Wallet.isEternlAvailable,
    isFlintAvailable: Wallet.isFlintAvailable,
    isGeroAvailable: Wallet.isGeroAvailable,
    isLodeAvailable: Wallet.isLodeAvailable,
    isNamiAvailable: Wallet.isNamiAvailable,
    PrivatePaymentKey: Wallet_Key.PrivatePaymentKey,
    PrivateStakeKey: Wallet_Key.PrivateStakeKey,
    privateKeysToKeyWallet: Wallet_Key.privateKeysToKeyWallet,
    formatPaymentKey: Wallet_KeyFile.formatPaymentKey,
    formatStakeKey: Wallet_KeyFile.formatStakeKey,
    PrivatePaymentKeyFile: Wallet_Spec.PrivatePaymentKeyFile,
    PrivatePaymentKeyValue: Wallet_Spec.PrivatePaymentKeyValue,
    PrivateStakeKeyFile: Wallet_Spec.PrivateStakeKeyFile,
    PrivateStakeKeyValue: Wallet_Spec.PrivateStakeKeyValue,
    ConnectToEternl: Wallet_Spec.ConnectToEternl,
    ConnectToFlint: Wallet_Spec.ConnectToFlint,
    ConnectToGero: Wallet_Spec.ConnectToGero,
    ConnectToLode: Wallet_Spec.ConnectToLode,
    ConnectToNami: Wallet_Spec.ConnectToNami,
    UseKeys: Wallet_Spec.UseKeys
};
