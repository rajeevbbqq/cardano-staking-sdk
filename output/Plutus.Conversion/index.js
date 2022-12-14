// Generated by purs version 0.14.5
"use strict";
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Hashing = require("../Hashing/index.js");
var Plutus_Conversion_Address = require("../Plutus.Conversion.Address/index.js");
var Plutus_Conversion_Value = require("../Plutus.Conversion.Value/index.js");
var toPlutusTxOutput = function (cardanoTxOut) {
    var rec = Data_Newtype.unwrap()(cardanoTxOut);
    return Control_Bind.bind(Data_Maybe.bindMaybe)(Plutus_Conversion_Address.toPlutusAddress(rec.address))(function (address) {
        var referenceScript = Control_Bind.bindFlipped(Data_Maybe.bindMaybe)(Hashing.scriptRefHash)(rec.scriptRef);
        var amount = Plutus_Conversion_Value.toPlutusValue(rec.amount);
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Newtype.wrap()({
            address: address,
            amount: amount,
            datum: rec.datum,
            referenceScript: referenceScript
        }));
    });
};
var toPlutusTxOutputWithRefScript = function (cTxOutput) {
    return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(toPlutusTxOutput(cTxOutput))((function () {
        var $6 = Data_Newtype.wrap();
        return function ($7) {
            return $6((function (v) {
                return {
                    output: v,
                    scriptRef: (Data_Newtype.unwrap()(cTxOutput)).scriptRef
                };
            })($7));
        };
    })());
};
var toPlutusTxUnspentOutput = function (txUnspentOutput) {
    var rec = Data_Newtype.unwrap()(txUnspentOutput);
    return Control_Bind.bind(Data_Maybe.bindMaybe)(toPlutusTxOutputWithRefScript(rec.output))(function (output) {
        return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(Data_Newtype.wrap()({
            input: rec.input,
            output: output
        }));
    });
};
var toPlutusUtxoMap = Data_Traversable.traverse(Data_Map_Internal.traversableMap)(Data_Maybe.applicativeMaybe)(toPlutusTxOutputWithRefScript);
var toPlutusCoin = (function () {
    var $8 = Data_Newtype.wrap();
    var $9 = Data_Newtype.unwrap();
    return function ($10) {
        return $8($9($10));
    };
})();
var fromPlutusTxOutput = function (networkId) {
    return function (scriptRef) {
        return function (plutusTxOut) {
            var rec = Data_Newtype.unwrap()(plutusTxOut);
            return Data_Newtype.wrap()({
                address: Plutus_Conversion_Address.fromPlutusAddress(networkId)(rec.address),
                amount: Plutus_Conversion_Value.fromPlutusValue(rec.amount),
                datum: rec.datum,
                scriptRef: scriptRef
            });
        };
    };
};
var fromPlutusTxOutputWithRefScript = function (networkId) {
    return function (v) {
        return fromPlutusTxOutput(networkId)(v.scriptRef)(v.output);
    };
};
var fromPlutusTxUnspentOutput = function (networkId) {
    return function (txUnspentOutput) {
        var rec = Data_Newtype.unwrap()(txUnspentOutput);
        return Data_Newtype.wrap()({
            input: rec.input,
            output: fromPlutusTxOutputWithRefScript(networkId)(rec.output)
        });
    };
};
var fromPlutusUtxoMap = function (networkId) {
    return Data_Functor.map(Data_Map_Internal.functorMap)(fromPlutusTxOutputWithRefScript(networkId));
};
var fromPlutusCoin = (function () {
    var $11 = Data_Newtype.wrap();
    var $12 = Data_Newtype.unwrap();
    return function ($13) {
        return $11($12($13));
    };
})();
module.exports = {
    fromPlutusCoin: fromPlutusCoin,
    toPlutusCoin: toPlutusCoin,
    fromPlutusTxOutput: fromPlutusTxOutput,
    toPlutusTxOutput: toPlutusTxOutput,
    fromPlutusTxOutputWithRefScript: fromPlutusTxOutputWithRefScript,
    toPlutusTxOutputWithRefScript: toPlutusTxOutputWithRefScript,
    fromPlutusTxUnspentOutput: fromPlutusTxUnspentOutput,
    toPlutusTxUnspentOutput: toPlutusTxUnspentOutput,
    fromPlutusUtxoMap: fromPlutusUtxoMap,
    toPlutusUtxoMap: toPlutusUtxoMap,
    fromPlutusAddress: Plutus_Conversion_Address.fromPlutusAddress,
    fromPlutusAddressWithNetworkTag: Plutus_Conversion_Address.fromPlutusAddressWithNetworkTag,
    toPlutusAddress: Plutus_Conversion_Address.toPlutusAddress,
    toPlutusAddressWithNetworkTag: Plutus_Conversion_Address.toPlutusAddressWithNetworkTag,
    fromPlutusValue: Plutus_Conversion_Value.fromPlutusValue,
    toPlutusValue: Plutus_Conversion_Value.toPlutusValue
};
