// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Cardano_Types_ScriptRef = require("../Cardano.Types.ScriptRef/index.js");
var Cardano_Types_Transaction = require("../Cardano.Types.Transaction/index.js");
var Cardano_Types_Value = require("../Cardano.Types.Value/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Apply = require("../Control.Apply/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Set = require("../Data.Set/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_UInt = require("../Data.UInt/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Deserialization_FromBytes = require("../Deserialization.FromBytes/index.js");
var Effect = require("../Effect/index.js");
var FfiHelpers = require("../FfiHelpers/index.js");
var Helpers = require("../Helpers/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var Serialization_AuxiliaryData = require("../Serialization.AuxiliaryData/index.js");
var Serialization_BigInt = require("../Serialization.BigInt/index.js");
var Serialization_Hash = require("../Serialization.Hash/index.js");
var Serialization_NativeScript = require("../Serialization.NativeScript/index.js");
var Serialization_PlutusData = require("../Serialization.PlutusData/index.js");
var Serialization_PlutusScript = require("../Serialization.PlutusScript/index.js");
var Serialization_WitnessSet = require("../Serialization.WitnessSet/index.js");
var ToData = require("../ToData/index.js");
var Types_BigNum = require("../Types.BigNum/index.js");
var Types_OutputDatum = require("../Types.OutputDatum/index.js");
var Types_Scripts = require("../Types.Scripts/index.js");
var Types_TokenName = require("../Types.TokenName/index.js");
var Untagged_Union = require("../Untagged.Union/index.js");
var serializeData = function (dictToData) {
    var $130 = Data_Functor.map(Data_Maybe.functorMaybe)((function () {
        var $133 = Data_Newtype.wrap();
        var $134 = Untagged_Union.asOneOf();
        return function ($135) {
            return $133($foreign.toBytes($134($135)));
        };
    })());
    var $131 = ToData.toData(dictToData);
    return function ($132) {
        return $130(Serialization_PlutusData.convertPlutusData($131($132)));
    };
};
var publicKeyFromBech32 = $foreign["_publicKeyFromBech32"](FfiHelpers.maybeFfiHelper);
var privateKeyFromBytes = $foreign["_privateKeyFromBytes"](FfiHelpers.maybeFfiHelper);
var mkUnitInterval = function (x) {
    return $foreign.newUnitInterval(x.numerator)(x.denominator);
};
var convertWithdrawals = function (mp) {
    return Control_Bind.bindFlipped(Effect.bindEffect)($foreign.newWithdrawals(FfiHelpers.containerHelper))(Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableArray)(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(mp))(function (v) {
        return Data_Functor.map(Effect.functorEffect)(Data_Tuple.Tuple.create(v.value0))(Helpers.fromJustEff("convertWithdrawals: Failed to convert BigNum")(Types_BigNum.fromBigInt(v.value1)));
    }));
};
var convertValue = function (val) {
    var m = Cardano_Types_Value["getNonAdaAsset'"](val);
    var lovelace = Cardano_Types_Value["valueToCoin'"](val);
    return function __do() {
        var multiasset = $foreign.newMultiAsset();
        Data_FoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data_Map_Internal.foldableWithIndexMap)(m)(function (scriptHashBytes$prime) {
            return function (values) {
                var mScripthash = Serialization_Hash.scriptHashFromBytes(Data_Newtype.wrap()(Cardano_Types_Value.getCurrencySymbol(scriptHashBytes$prime)));
                return function __do() {
                    var scripthash = Helpers.fromJustEff("scriptHashFromBytes failed while converting value")(mScripthash)();
                    var assets = $foreign.newAssets();
                    Data_FoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data_Map_Internal.foldableWithIndexMap)(values)(function (tokenName$prime) {
                        return function (bigIntValue) {
                            var tokenName = Types_TokenName.getTokenName(tokenName$prime);
                            return function __do() {
                                var assetName = $foreign.newAssetName(tokenName)();
                                var value = Helpers.fromJustEff("convertValue: number must not be negative")(Types_BigNum.fromBigInt(bigIntValue))();
                                return $foreign.insertAssets(assets)(assetName)(value)();
                            };
                        };
                    })();
                    return $foreign.insertMultiAsset(multiasset)(scripthash)(assets)();
                };
            };
        })();
        var value = $foreign.newValueFromAssets(multiasset)();
        Control_Bind.bindFlipped(Effect.bindEffect)($foreign.valueSetCoin(value))(Helpers.fromJustEff("convertValue: coin value must not be negative")(Types_BigNum.fromBigInt(lovelace)))();
        return value;
    };
};
var convertTxInput = function (v) {
    return function __do() {
        var tx_hash = Deserialization_FromBytes.fromBytesEffect(Deserialization_FromBytes.fromBytesTransactionHash)(Data_Newtype.unwrap()(v.transactionId))();
        return $foreign.newTransactionInput(tx_hash)(v.index)();
    };
};
var convertTxInputs = function (dictFoldable) {
    return function (fInputs) {
        return function __do() {
            var inputs = $foreign.newTransactionInputs();
            Data_Foldable.traverse_(Effect.applicativeEffect)(dictFoldable)(Control_Bind.composeKleisli(Effect.bindEffect)(convertTxInput)($foreign.addTransactionInput(inputs)))(fInputs)();
            return inputs;
        };
    };
};
var convertScriptRef = function (v) {
    if (v instanceof Cardano_Types_ScriptRef.NativeScriptRef) {
        return Data_Functor.map(Effect.functorEffect)($foreign.scriptRefNewNativeScript)(Helpers.fromJustEff("convertScriptRef")(Serialization_NativeScript.convertNativeScript(v.value0)));
    };
    if (v instanceof Cardano_Types_ScriptRef.PlutusScriptRef) {
        return Control_Applicative.pure(Effect.applicativeEffect)($foreign.scriptRefNewPlutusScript(Serialization_PlutusScript.convertPlutusScript(v.value0)));
    };
    throw new Error("Failed pattern match at Serialization (line 816, column 1 - line 816, column 52): " + [ v.constructor.name ]);
};
var convertTxOutput = function (v) {
    return function __do() {
        var value = convertValue(v.amount)();
        var txo = $foreign.newTransactionOutput(v.address)(value)();
        (function () {
            if (v.datum instanceof Types_OutputDatum.NoOutputDatum) {
                return Data_Unit.unit;
            };
            if (v.datum instanceof Types_OutputDatum.OutputDatumHash) {
                return Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(Deserialization_FromBytes.fromBytes(Deserialization_FromBytes.fromBytesDataHash)(Data_Newtype.unwrap()(v.datum.value0)))($foreign.transactionOutputSetDataHash(txo))();
            };
            if (v.datum instanceof Types_OutputDatum.OutputDatum) {
                return Control_Bind.bindFlipped(Effect.bindEffect)($foreign.transactionOutputSetPlutusData(txo))(Helpers.fromJustEff("convertTxOutput")(Serialization_PlutusData.convertPlutusData(Data_Newtype.unwrap()(v.datum.value0))))();
            };
            throw new Error("Failed pattern match at Serialization (line 803, column 3 - line 811, column 50): " + [ v.datum.constructor.name ]);
        })();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.scriptRef)(Control_Bind.composeKleisli(Effect.bindEffect)(convertScriptRef)($foreign.transactionOutputSetScriptRef(txo)))();
        return txo;
    };
};
var convertTransactionUnspentOutput = function (v) {
    return function __do() {
        var input$prime = convertTxInput(v.input)();
        var output$prime = convertTxOutput(v.output)();
        return $foreign.newTransactionUnspentOutput(input$prime)(output$prime)();
    };
};
var convertTxOutputs = function (arrOutputs) {
    return function __do() {
        var outputs = $foreign.newTransactionOutputs();
        Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(Control_Bind.composeKleisli(Effect.bindEffect)(convertTxOutput)($foreign.addTransactionOutput(outputs)))(arrOutputs)();
        return outputs;
    };
};
var convertRelays = function (relays) {
    return Data_Functor.map(Effect.functorEffect)($foreign.packRelays(FfiHelpers.containerHelper))(Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableArray)(relays)(function (relay) {
        if (relay instanceof Cardano_Types_Transaction.SingleHostAddr) {
            return function __do() {
                var ipv4$prime = Data_Functor.map(Effect.functorEffect)(Untagged_Union.maybeToUor)(Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.unwrap())(relay.value0.ipv4))($foreign.newIpv4))();
                var ipv6$prime = Data_Functor.map(Effect.functorEffect)(Untagged_Union.maybeToUor)(Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableMaybe)(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.unwrap())(relay.value0.ipv6))($foreign.newIpv6))();
                return $foreign.newSingleHostAddr(Untagged_Union.maybeToUor(relay.value0.port))(ipv4$prime)(ipv6$prime)();
            };
        };
        if (relay instanceof Cardano_Types_Transaction.SingleHostName) {
            return $foreign.newSingleHostName(Untagged_Union.maybeToUor(relay.value0.port))(relay.value0.dnsName);
        };
        if (relay instanceof Cardano_Types_Transaction.MultiHostName) {
            return $foreign.newMultiHostName(relay.value0.dnsName);
        };
        throw new Error("Failed pattern match at Serialization (line 739, column 55 - line 747, column 31): " + [ relay.constructor.name ]);
    }));
};
var convertPoolMetadata = function (v) {
    return $foreign.newPoolMetadata(v.url)(v.hash);
};
var convertNetworkId = function (v) {
    if (v instanceof Serialization_Address.TestnetId) {
        return $foreign.networkIdTestnet;
    };
    if (v instanceof Serialization_Address.MainnetId) {
        return $foreign.networkIdMainnet;
    };
    throw new Error("Failed pattern match at Serialization (line 750, column 20 - line 752, column 34): " + [ v.constructor.name ]);
};
var convertMint = function (v) {
    var m = Cardano_Types_Value.unwrapNonAdaAsset(v);
    return function __do() {
        var mint = $foreign.newMint();
        Data_FoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data_Map_Internal.foldableWithIndexMap)(m)(function (scriptHashBytes$prime) {
            return function (values) {
                var mScripthash = Serialization_Hash.scriptHashFromBytes(Data_Newtype.wrap()(Cardano_Types_Value.getCurrencySymbol(scriptHashBytes$prime)));
                return function __do() {
                    var scripthash = Helpers.fromJustEff("scriptHashFromBytes failed while converting value")(mScripthash)();
                    var assets = $foreign.newMintAssets();
                    Data_FoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data_Map_Internal.foldableWithIndexMap)(values)(function (tokenName$prime) {
                        return function (bigIntValue) {
                            var tokenName = Types_TokenName.getTokenName(tokenName$prime);
                            return function __do() {
                                var assetName = $foreign.newAssetName(tokenName)();
                                var bigInt = Helpers.fromJustEff("convertMint: failed to convert BigInt")(Serialization_BigInt.convertBigInt(bigIntValue))();
                                var $$int = Helpers.fromJustEff("convertMint: numeric overflow or underflow")($foreign["_bigIntToInt"](FfiHelpers.maybeFfiHelper)(bigInt))();
                                return $foreign.insertMintAsset(assets)(assetName)($$int)();
                            };
                        };
                    })();
                    return $foreign.insertMintAssets(mint)(scripthash)(assets)();
                };
            };
        })();
        return mint;
    };
};
var convertMIRToStakeCredentials = function (v) {
    return $foreign.newMIRToStakeCredentials(FfiHelpers.containerHelper)(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(v));
};
var convertMoveInstantaneousReward = function (v) {
    if (v instanceof Cardano_Types_Transaction.ToOtherPot) {
        return $foreign.newMoveInstantaneousRewardToOtherPot(v.value0.pot)(v.value0.amount);
    };
    if (v instanceof Cardano_Types_Transaction.ToStakeCreds) {
        return Control_Bind.bind(Effect.bindEffect)(convertMIRToStakeCredentials(v.value0.amounts))($foreign.newMoveInstantaneousRewardToStakeCreds(v.value0.pot));
    };
    throw new Error("Failed pattern match at Serialization (line 724, column 1 - line 725, column 65): " + [ v.constructor.name ]);
};
var convertExUnitPrices = function (v) {
    return Control_Bind.join(Effect.bindEffect)(Control_Apply.apply(Effect.applyEffect)(Data_Functor.map(Effect.functorEffect)($foreign.newExUnitPrices)(mkUnitInterval(v.memPrice)))(mkUnitInterval(v.stepPrice)));
};
var convertCostModel = function (v) {
    return function __do() {
        var costModel = $foreign.newCostModel();
        Data_FoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data_FoldableWithIndex.foldableWithIndexArray)(v)(function (operation) {
            return function (cost) {
                return $foreign.costModelSetCost(costModel)(operation)(cost);
            };
        })();
        return costModel;
    };
};
var convertCostmdls = function (v) {
    return function __do() {
        var costmdls = $foreign.newCostmdls();
        Data_FoldableWithIndex.forWithIndex_(Effect.applicativeEffect)(Data_Map_Internal.foldableWithIndexMap)(v)(function (language) {
            return function (costModel) {
                return function __do() {
                    var language$prime = (function () {
                        if (language instanceof Types_Scripts.PlutusV1) {
                            return $foreign.newPlutusV1();
                        };
                        if (language instanceof Types_Scripts.PlutusV2) {
                            return $foreign.newPlutusV2();
                        };
                        throw new Error("Failed pattern match at Serialization (line 854, column 18 - line 856, column 32): " + [ language.constructor.name ]);
                    })();
                    var costModel$prime = convertCostModel(costModel)();
                    return $foreign.costmdlsSetCostModel(costmdls)(language$prime)(costModel$prime)();
                };
            };
        })();
        return costmdls;
    };
};
var convertProtocolParamUpdate = function (v) {
    return function __do() {
        var ppu = $foreign.newProtocolParamUpdate();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.minfeeA)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)($foreign.ppuSetMinfeeA(ppu))((function () {
            var $136 = Helpers.fromJustEff("convertProtocolParamUpdate: min_fee_a must not be negative");
            var $137 = Data_Newtype.unwrap();
            return function ($138) {
                return $136(Types_BigNum.fromBigInt($137($138)));
            };
        })()))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.minfeeB)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)($foreign.ppuSetMinfeeB(ppu))((function () {
            var $139 = Helpers.fromJustEff("convertProtocolParamUpdate: min_fee_b must not be negative");
            var $140 = Data_Newtype.unwrap();
            return function ($141) {
                return $139(Types_BigNum.fromBigInt($140($141)));
            };
        })()))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxBlockBodySize)((function () {
            var $142 = $foreign.ppuSetMaxBlockBodySize(ppu);
            return function ($143) {
                return $142(Data_UInt.toInt($143));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxTxSize)((function () {
            var $144 = $foreign.ppuSetMaxTxSize(ppu);
            return function ($145) {
                return $144(Data_UInt.toInt($145));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxBlockHeaderSize)((function () {
            var $146 = $foreign.ppuSetMaxBlockHeaderSize(ppu);
            return function ($147) {
                return $146(Data_UInt.toInt($147));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.keyDeposit)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)($foreign.ppuSetKeyDeposit(ppu))((function () {
            var $148 = Helpers.fromJustEff("convertProtocolParamUpdate: key_deposit must not be negative");
            var $149 = Data_Newtype.unwrap();
            return function ($150) {
                return $148(Types_BigNum.fromBigInt($149($150)));
            };
        })()))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.poolDeposit)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)($foreign.ppuSetPoolDeposit(ppu))((function () {
            var $151 = Helpers.fromJustEff("convertProtocolParamUpdate: pool_deposit must not be negative");
            var $152 = Data_Newtype.unwrap();
            return function ($153) {
                return $151(Types_BigNum.fromBigInt($152($153)));
            };
        })()))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxEpoch)((function () {
            var $154 = $foreign.ppuSetMaxEpoch(ppu);
            var $155 = Data_Newtype.unwrap();
            return function ($156) {
                return $154(Data_UInt.toInt($155($156)));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.nOpt)((function () {
            var $157 = $foreign.ppuSetNOpt(ppu);
            return function ($158) {
                return $157(Data_UInt.toInt($158));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.poolPledgeInfluence)(Control_Bind.composeKleisli(Effect.bindEffect)(mkUnitInterval)($foreign.ppuSetPoolPledgeInfluence(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.expansionRate)(Control_Bind.composeKleisli(Effect.bindEffect)(mkUnitInterval)($foreign.ppuSetExpansionRate(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.treasuryGrowthRate)(Control_Bind.composeKleisli(Effect.bindEffect)(mkUnitInterval)($foreign.ppuSetTreasuryGrowthRate(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.protocolVersion)(function (pv) {
            return Control_Bind.bindFlipped(Effect.bindEffect)($foreign.ppuSetProtocolVersion(ppu))($foreign.newProtocolVersion(Data_UInt.toInt(pv.major))(Data_UInt.toInt(pv.minor)));
        })();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.minPoolCost)($foreign.ppuSetMinPoolCost(ppu))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.adaPerUtxoByte)($foreign.ppuSetAdaPerUtxoByte(ppu))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.costModels)(Control_Bind.composeKleisli(Effect.bindEffect)(convertCostmdls)($foreign.ppuSetCostModels(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.executionCosts)(Control_Bind.composeKleisli(Effect.bindEffect)(convertExUnitPrices)($foreign.ppuSetExecutionCosts(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxTxExUnits)(Control_Bind.composeKleisli(Effect.bindEffect)(Serialization_WitnessSet.convertExUnits)($foreign.ppuSetMaxTxExUnits(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxBlockExUnits)(Control_Bind.composeKleisli(Effect.bindEffect)(Serialization_WitnessSet.convertExUnits)($foreign.ppuSetMaxBlockExUnits(ppu)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.maxValueSize)((function () {
            var $159 = $foreign.ppuSetMaxValueSize(ppu);
            return function ($160) {
                return $159(Data_UInt.toInt($160));
            };
        })())();
        return ppu;
    };
};
var convertProposedProtocolParameterUpdates = function (ppus) {
    return Control_Bind.bindFlipped(Effect.bindEffect)($foreign.newProposedProtocolParameterUpdates(FfiHelpers.containerHelper))(Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableArray)(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(Data_Newtype.unwrap()(ppus)))(function (v) {
        return Control_Apply.apply(Effect.applyEffect)(Data_Functor.map(Effect.functorEffect)(Data_Tuple.Tuple.create)($foreign.newGenesisHash(Data_Newtype.unwrap()(v.value0))))(convertProtocolParamUpdate(v.value1));
    }));
};
var convertUpdate = function (v) {
    return function __do() {
        var ppUpdates = convertProposedProtocolParameterUpdates(v.proposedProtocolParameterUpdates)();
        return $foreign.newUpdate(ppUpdates)(Data_UInt.toInt(Data_Newtype.unwrap()(v.epoch)))();
    };
};
var hashScriptData = function (cms) {
    return function (rs) {
        return function (ps) {
            return function __do() {
                var rs$prime = $foreign.newRedeemers();
                var cms$prime = convertCostmdls(cms)();
                Data_Foldable.traverse_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(Control_Bind.composeKleisliFlipped(Effect.bindEffect)($foreign.addRedeemer(rs$prime))(Serialization_WitnessSet.convertRedeemer))(rs)();
                if (ps.length === 0) {
                    return $foreign["_hashScriptDataNoDatums"](rs$prime)(cms$prime)();
                };
                return Control_Bind.bindFlipped(Effect.bindEffect)($foreign["_hashScriptData"](rs$prime)(cms$prime))(Helpers.fromJustEff("failed to convert datums")(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(Serialization_PlutusData.convertPlutusData)(ps)))();
            };
        };
    };
};
var convertCert = function (v) {
    if (v instanceof Cardano_Types_Transaction.StakeRegistration) {
        return $foreign.newStakeRegistrationCertificate(v.value0);
    };
    if (v instanceof Cardano_Types_Transaction.StakeDeregistration) {
        return $foreign.newStakeDeregistrationCertificate(v.value0);
    };
    if (v instanceof Cardano_Types_Transaction.StakeDelegation) {
        return $foreign.newStakeDelegationCertificate(v.value0)(v.value1);
    };
    if (v instanceof Cardano_Types_Transaction.PoolRegistration) {
        return function __do() {
            var margin$prime = $foreign.newUnitInterval(v.value0.margin.numerator)(v.value0.margin.denominator)();
            var poolOwners$prime = $foreign.convertPoolOwners(FfiHelpers.containerHelper)(v.value0.poolOwners)();
            var relays$prime = convertRelays(v.value0.relays)();
            var poolMetadata$prime = Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableMaybe)(v.value0.poolMetadata)(convertPoolMetadata)();
            return $foreign.newPoolRegistrationCertificate(v.value0.operator)(v.value0.vrfKeyhash)(v.value0.pledge)(v.value0.cost)(margin$prime)(v.value0.rewardAccount)(poolOwners$prime)(relays$prime)(Untagged_Union.maybeToUor(poolMetadata$prime))();
        };
    };
    if (v instanceof Cardano_Types_Transaction.PoolRetirement) {
        return $foreign.newPoolRetirementCertificate(v.value0.poolKeyhash)(Data_UInt.toInt(Data_Newtype.unwrap()(v.value0.epoch)));
    };
    if (v instanceof Cardano_Types_Transaction.GenesisKeyDelegation) {
        return Control_Bind.join(Effect.bindEffect)(Control_Apply.apply(Effect.applyEffect)(Control_Apply.apply(Effect.applyEffect)(Data_Functor.map(Effect.functorEffect)($foreign.newGenesisKeyDelegationCertificate)($foreign.newGenesisHash(v.value0.genesisHash)))($foreign.newGenesisDelegateHash(v.value0.genesisDelegateHash)))(Control_Applicative.pure(Effect.applicativeEffect)(v.value0.vrfKeyhash)));
    };
    if (v instanceof Cardano_Types_Transaction.MoveInstantaneousRewardsCert) {
        return Control_Bind.bindFlipped(Effect.bindEffect)($foreign.newMoveInstantaneousRewardsCertificate)(convertMoveInstantaneousReward(v.value0));
    };
    throw new Error("Failed pattern match at Serialization (line 676, column 15 - line 717, column 41): " + [ v.constructor.name ]);
};
var convertCerts = function (certs) {
    return function __do() {
        var certificates = $foreign.newCertificates();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableArray)(certs)(Control_Bind.composeKleisli(Effect.bindEffect)(convertCert)($foreign.addCert(certificates)))();
        return certificates;
    };
};
var convertTxBody = function (v) {
    return function __do() {
        var inputs = convertTxInputs(Data_Set.foldableSet)(v.inputs)();
        var outputs = convertTxOutputs(v.outputs)();
        var fee = Helpers.fromJustEff("Failed to convert fee")(Types_BigNum.fromBigInt(Data_Newtype.unwrap()(v.fee)))();
        var txBody = $foreign.newTransactionBody(inputs)(outputs)(fee)();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.ttl)((function () {
            var $161 = $foreign.setTxBodyTtl(txBody);
            var $162 = Data_Newtype.unwrap();
            return function ($163) {
                return $161($162($163));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.certs)(Control_Bind.composeKleisli(Effect.bindEffect)(convertCerts)($foreign.setTxBodyCerts(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.withdrawals)(Control_Bind.composeKleisli(Effect.bindEffect)(convertWithdrawals)($foreign.setTxBodyWithdrawals(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.update)(Control_Bind.composeKleisli(Effect.bindEffect)(convertUpdate)($foreign.setTxBodyUpdate(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.auxiliaryDataHash)((function () {
            var $164 = $foreign.transactionBodySetAuxiliaryDataHash(txBody);
            var $165 = Data_Newtype.unwrap();
            return function ($166) {
                return $164($165($166));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.validityStartInterval)((function () {
            var $167 = $foreign.transactionBodySetValidityStartInterval(txBody);
            var $168 = Data_Newtype.unwrap();
            return function ($169) {
                return $167(Types_BigNum.fromStringUnsafe(Types_BigNum.toString($168($169))));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.requiredSigners)((function () {
            var $170 = $foreign.transactionBodySetRequiredSigners(FfiHelpers.containerHelper)(txBody);
            var $171 = Data_Functor.map(Data_Functor.functorArray)(Data_Newtype.unwrap());
            return function ($172) {
                return $170($171($172));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.auxiliaryDataHash)((function () {
            var $173 = $foreign.transactionBodySetAuxiliaryDataHash(txBody);
            var $174 = Data_Newtype.unwrap();
            return function ($175) {
                return $173($174($175));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.networkId)(Control_Bind.composeKleisli(Effect.bindEffect)(convertNetworkId)($foreign.setTxBodyNetworkId(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.mint)(Control_Bind.composeKleisli(Effect.bindEffect)(convertMint)($foreign.setTxBodyMint(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.scriptDataHash)(Control_Bind.composeKleisli(Effect.bindEffect)((function () {
            var $176 = Data_Newtype.wrap();
            var $177 = Data_Newtype.unwrap();
            return function ($178) {
                return $foreign.newScriptDataHashFromBytes($176($177($178)));
            };
        })())($foreign.setTxBodyScriptDataHash(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.collateral)(Control_Bind.composeKleisli(Effect.bindEffect)(convertTxInputs(Data_Foldable.foldableArray))($foreign.setTxBodyCollateral(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.requiredSigners)((function () {
            var $179 = $foreign.transactionBodySetRequiredSigners(FfiHelpers.containerHelper)(txBody);
            var $180 = Data_Functor.map(Data_Functor.functorArray)(Data_Newtype.unwrap());
            return function ($181) {
                return $179($180($181));
            };
        })())();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.networkId)(Control_Bind.composeKleisli(Effect.bindEffect)(convertNetworkId)($foreign.setTxBodyNetworkId(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.collateralReturn)(Control_Bind.composeKleisli(Effect.bindEffect)(convertTxOutput)($foreign.setTxBodyCollateralReturn(txBody)))();
        Data_Foldable.for_(Effect.applicativeEffect)(Data_Foldable.foldableMaybe)(v.totalCollateral)(Control_Bind.composeKleisli(Effect.bindEffect)((function () {
            var $182 = Helpers.fromJustEff("Failed to convert fee");
            var $183 = Data_Newtype.unwrap();
            return function ($184) {
                return $182(Types_BigNum.fromBigInt($183($184)));
            };
        })())($foreign.setTxBodyTotalCollateral(txBody)))();
        (function () {
            var $122 = Data_Foldable["null"](Data_Set.foldableSet)(v.referenceInputs);
            if ($122) {
                return Data_Unit.unit;
            };
            return Control_Bind.bind(Effect.bindEffect)(convertTxInputs(Data_Set.foldableSet)(v.referenceInputs))($foreign.setTxBodyReferenceInputs(txBody))();
        })();
        return txBody;
    };
};
var convertTransaction = function (v) {
    return function __do() {
        var txBody = convertTxBody(v.body)();
        var ws = Serialization_WitnessSet.convertWitnessSet(v.witnessSet)();
        var mbAuxiliaryData = Data_Traversable["for"](Effect.applicativeEffect)(Data_Traversable.traversableMaybe)(v.auxiliaryData)(Serialization_AuxiliaryData.convertAuxiliaryData)();
        var tx = (function () {
            if (mbAuxiliaryData instanceof Data_Maybe.Nothing) {
                return $foreign.newTransaction_(txBody)(ws)();
            };
            if (mbAuxiliaryData instanceof Data_Maybe.Just) {
                return $foreign.newTransaction(txBody)(ws)(mbAuxiliaryData.value0)();
            };
            throw new Error("Failed pattern match at Serialization (line 556, column 11 - line 558, column 45): " + [ mbAuxiliaryData.constructor.name ]);
        })();
        $foreign.setTxIsValid(tx)(v.isValid)();
        return tx;
    };
};
var bytesFromPrivateKey = $foreign["_bytesFromPrivateKey"](FfiHelpers.maybeFfiHelper);
module.exports = {
    bytesFromPrivateKey: bytesFromPrivateKey,
    convertExUnitPrices: convertExUnitPrices,
    convertTransaction: convertTransaction,
    convertTxBody: convertTxBody,
    convertTxInput: convertTxInput,
    convertTxOutput: convertTxOutput,
    convertTransactionUnspentOutput: convertTransactionUnspentOutput,
    convertValue: convertValue,
    serializeData: serializeData,
    hashScriptData: hashScriptData,
    publicKeyFromBech32: publicKeyFromBech32,
    privateKeyFromBytes: privateKeyFromBytes,
    defaultCostmdls: $foreign.defaultCostmdls,
    toBytes: $foreign.toBytes,
    newTransactionUnspentOutputFromBytes: $foreign.newTransactionUnspentOutputFromBytes,
    newTransactionWitnessSetFromBytes: $foreign.newTransactionWitnessSetFromBytes,
    hashTransaction: $foreign.hashTransaction,
    publicKeyHash: $foreign.publicKeyHash,
    publicKeyFromPrivateKey: $foreign.publicKeyFromPrivateKey,
    makeVkeywitness: $foreign.makeVkeywitness
};
