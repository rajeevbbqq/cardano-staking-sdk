// Generated by purs version 0.14.5
"use strict";
var Contract_Address = require("../Contract.Address/index.js");
var Contract_Log = require("../Contract.Log/index.js");
var Contract_Monad = require("../Contract.Monad/index.js");
var Contract_PlutusData = require("../Contract.PlutusData/index.js");
var Contract_Prelude = require("../Contract.Prelude/index.js");
var Contract_Utxos = require("../Contract.Utxos/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var FromData = require("../FromData/index.js");
var Plutus_Conversion_Address = require("../Plutus.Conversion.Address/index.js");
var Plutus_Types_Address = require("../Plutus.Types.Address/index.js");
var Plutus_Types_Transaction = require("../Plutus.Types.Transaction/index.js");
var Plutus_Types_Value = require("../Plutus.Types.Value/index.js");
var Scripts = require("../Scripts/index.js");
var Scripts_PoolValidator = require("../Scripts.PoolValidator/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var Settings = require("../Settings/index.js");
var ToData = require("../ToData/index.js");
var Types_Datum = require("../Types.Datum/index.js");
var Types_Interval = require("../Types.Interval/index.js");
var Types_Natural = require("../Types.Natural/index.js");
var Types_PubKeyHash = require("../Types.PubKeyHash/index.js");
var Types_Rational = require("../Types.Rational/index.js");
var Types_Redeemer = require("../Types.Redeemer/index.js");
var Types_ScriptLookups = require("../Types.ScriptLookups/index.js");
var Types_Scripts = require("../Types.Scripts/index.js");
var Types_TokenName = require("../Types.TokenName/index.js");
var Types_Transaction = require("../Types.Transaction/index.js");
var Types_TxConstraints = require("../Types.TxConstraints/index.js");
var UnbondedStaking_Types = require("../UnbondedStaking.Types/index.js");
var UnbondedStaking_Utils = require("../UnbondedStaking.Utils/index.js");
var Utils = require("../Utils/index.js");
var mkEntryUpdateList = function (v) {
    return function (valHash) {
        return function (v1) {
            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("mkEntryUpdateList: Could not get Entry Datum Hash")(Utils.getUtxoDatumHash(v1.value1.value1)))(function (dHash) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Types_Transaction.showDataHash)("mkEntryUpdateList: datum hash")(dHash))(function () {
                    return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("mkEntryUpdateList: Cannot get Entry's datum")(Contract_PlutusData.getDatumByHash(dHash)))(function (listDatum) {
                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("mkEntryUpdateList: Cannot extract NFT State datum")(FromData.fromData(UnbondedStaking_Types.fromDataUnbondedStakingDa)(Data_Newtype.unwrap()(listDatum))))(function (v2) {
                            if (v2 instanceof UnbondedStaking_Types.EntryDatum) {
                                var e = Data_Newtype.unwrap()(v2.value0.entry);
                                return Control_Bind.bind(Contract_Monad.bindContract)(UnbondedStaking_Utils.calculateRewards(e.rewards)(e.totalRewards)(e.deposited)(e.newDeposit)(e.totalDeposited))(function (calculatedRewards) {
                                    return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("mkEntryUpdateList: Could not create token name for user")(Types_TokenName.mkTokenName(e.key)))(function (assocListTn) {
                                        var updatedRewards = Utils.roundUp(calculatedRewards);
                                        var updatedTotalDeposited = Data_Semiring.add(Data_BigInt.semiringBigInt)(e.deposited)(updatedRewards);
                                        var incrementsRat = Utils.mkRatUnsafe(Types_Rational.reduce(Types_Rational.rationalComponentBigInt)(Types_Natural.toBigInt(v.increments))(Data_Semiring.one(Data_BigInt.semiringBigInt)));
                                        var updatedTotalRewards = Data_Semiring.mul(Data_BigInt.semiringBigInt)(updatedTotalDeposited)(Utils.roundUp(Data_Semiring.mul(Types_Rational.semiringRational)(v.interest)(incrementsRat)));
                                        var valRedeemer = Types_Redeemer.Redeemer(ToData.toData(UnbondedStaking_Types.toDataUnbondedStakingActi)(new UnbondedStaking_Types.AdminAct({
                                            totalRewards: Types_Natural["fromBigInt'"](updatedTotalRewards),
                                            totalDeposited: Types_Natural["fromBigInt'"](updatedTotalDeposited)
                                        })));
                                        var entryValue = Plutus_Types_Value.singleton(v.assocListCs)(assocListTn)(Data_Semiring.one(Data_BigInt.semiringBigInt));
                                        var entryDatum = Types_Datum.Datum(ToData.toData(UnbondedStaking_Types.toDataUnbondedStakingDatu)(new UnbondedStaking_Types.EntryDatum({
                                            entry: UnbondedStaking_Types.Entry({
                                                key: e.key,
                                                deposited: e.deposited,
                                                newDeposit: Data_Semiring.zero(Data_BigInt.semiringBigInt),
                                                rewards: Utils.mkRatUnsafe(Types_Rational.reduce(Types_Rational.rationalComponentBigInt)(updatedRewards)(Data_Semiring.one(Data_BigInt.semiringBigInt))),
                                                totalRewards: updatedTotalRewards,
                                                totalDeposited: updatedTotalDeposited,
                                                open: e.open,
                                                next: e.next
                                            })
                                        })));
                                        var assetParams = Data_Newtype.unwrap()(v.unbondedAssetClass);
                                        var assetDatum = Types_Datum.Datum(ToData.toData(UnbondedStaking_Types.toDataUnbondedStakingDatu)(UnbondedStaking_Types.AssetDatum.value));
                                        var depositValue = Plutus_Types_Value.singleton(assetParams.currencySymbol)(assetParams.tokenName)(updatedRewards);
                                        var constraints = Contract_Prelude.mconcat(Data_Foldable.foldableArray)(Types_TxConstraints.monoidTxConstraints)([ Utils.mustPayToScript(valHash)(assetDatum)(depositValue), Utils.mustPayToScript(valHash)(entryDatum)(entryValue), Types_TxConstraints.mustSpendScriptOutput(v1.value1.value0)(valRedeemer) ]);
                                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("mkEntryUpdateList: Could not create state datum lookup")(Types_ScriptLookups.datum(entryDatum)))(function (entryDatumLookup) {
                                            return Control_Applicative.pure(Contract_Monad.applicativeContract)(new Data_Tuple.Tuple(constraints, entryDatumLookup));
                                        });
                                    });
                                });
                            };
                            return Contract_Monad.throwContractError(Data_Show.showString)("mkEntryUpdateList: Datum not Entry constructor");
                        });
                    });
                });
            });
        };
    };
};
var depositUnbondedPoolContract = function (v) {
    return function (batchSize) {
        return function (depositList) {
            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Address.getNetworkId)(function (networkId) {
                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("depositUnbondedPoolContract: Cannot get user's pkh")(Contract_Address.ownPaymentPubKeyHash))(function (userPkh) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Control_Applicative.unless(Contract_Monad.applicativeContract)(Data_Eq.eq(Types_PubKeyHash.eqPaymentPubKeyHash)(userPkh)(v.admin))(Contract_Monad.throwContractError(Data_Show.showString)("depositUnbondedPoolContract: Admin is not current user")))(function () {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Types_PubKeyHash.showPaymentPubKeyHash)("depositUnbondedPoolContract: Admin PaymentPubKeyHash")(userPkh))(function () {
                            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("depositUnbondedPoolContract: Cannot get wallet Address")(Contract_Address.getWalletAddress))(function (adminAddr) {
                                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("depositUnbondedPoolContract: Cannot get user Utxos")(Contract_Utxos.utxosAt(adminAddr)))(function (adminUtxos) {
                                    return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad["liftedE'"]("depositUnbondedPoolContract: Cannot create validator")(Scripts_PoolValidator.mkUnbondedPoolValidator(v)))(function (validator) {
                                        var valHash = Scripts.validatorHash(validator);
                                        return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Types_Scripts.showValidatorHash)("depositUnbondedPoolContract: validatorHash")(valHash))(function () {
                                            var poolAddr = Plutus_Types_Address.scriptHashAddress(valHash);
                                            return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Serialization_Address.showAddress)("depositUnbondedPoolContract: Pool address")(Plutus_Conversion_Address.fromPlutusAddress(networkId)(poolAddr)))(function () {
                                                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("depositUnbondedPoolContract: Cannot get pool's utxos at pool address")(Contract_Utxos.utxosAt(poolAddr)))(function (unbondedPoolUtxos) {
                                                    return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Data_Map_Internal.showMap(Types_Transaction.showTransactionInput)(Plutus_Types_Transaction.showTransactionOutputWith))("depositUnbondedPoolContract: Pool UTXOs")(unbondedPoolUtxos))(function () {
                                                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("depositUnbondedPoolContract: Cannot create TokenName")(Settings.unbondedStakingTokenName))(function (tokenName) {
                                                            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("depositUnbondedPoolContract: Cannot get state utxo")(Utils.getUtxoWithNFT(unbondedPoolUtxos)(v.nftCs)(tokenName)))(function (v1) {
                                                                return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Types_Transaction.showTransactionInput)("depositUnbondedPoolContract: Pool's UTXO")(v1.value0))(function () {
                                                                    return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("depositUnbondedPoolContract: Could not get Pool UTXO's Datum Hash")(Utils.getUtxoDatumHash(v1.value1)))(function (poolDatumHash) {
                                                                        return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Types_Transaction.showDataHash)("depositUnbondedPoolContract: Pool's UTXO DatumHash")(poolDatumHash))(function () {
                                                                            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("depositUnbondedPoolContract: Cannot get datum")(Contract_PlutusData.getDatumByHash(poolDatumHash)))(function (poolDatum) {
                                                                                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("depositUnbondedPoolContract: Cannot extract NFT State datum")(FromData.fromData(UnbondedStaking_Types.fromDataUnbondedStakingDa)(Data_Newtype.unwrap()(poolDatum))))(function (v2) {
                                                                                    return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Contract_Log["logInfo'"](Contract_Monad.monadLoggerContract)("depositUnbondedPoolContract: Getting admin range..."))(function () {
                                                                                        return Control_Bind.bind(Contract_Monad.bindContract)(UnbondedStaking_Utils.getAdminTime(v))(function (v3) {
                                                                                            return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Data_Show.showString)("depositUnbondedPoolContract: Current time: ")(Data_Show.show(Types_Interval.showPOSIXTime)(v3.currTime)))(function () {
                                                                                                return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Types_Interval.showInterval(Types_Interval.showPOSIXTime))("depositUnbondedPoolContract: TX Range")(v3.range))(function () {
                                                                                                    if (v2 instanceof UnbondedStaking_Types.StateDatum && (v2.value0.maybeEntryName instanceof Data_Maybe.Just && v2.value0.open)) {
                                                                                                        return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Contract_Log["logInfo'"](Contract_Monad.monadLoggerContract)("depositUnbondedPoolContract: STAKE TYPE - StateDatum is StateDatum { maybeEntryName: Just ..., open: true }"))(function () {
                                                                                                            var lookups = Data_Semigroup.append(Types_ScriptLookups.semigroupScriptLookups)(Types_ScriptLookups.validator(validator))(Data_Semigroup.append(Types_ScriptLookups.semigroupScriptLookups)(Types_ScriptLookups.unspentOutputs(unbondedPoolUtxos))(Types_ScriptLookups.unspentOutputs(adminUtxos)));
                                                                                                            var constraints = Data_Semigroup.append(Types_TxConstraints.semigroupTxConstraints)(Types_TxConstraints.mustBeSignedBy(v.admin))(Types_TxConstraints.mustValidateIn(v3.range));
                                                                                                            var submitBatch = function (txBatch) {
                                                                                                                return Utils.submitTransaction(constraints)(lookups)(txBatch)(Settings.confirmationTimeout)(Settings.submissionAttempts);
                                                                                                            };
                                                                                                            var assocList = Utils.mkOnchainAssocList(v.assocListCs)(unbondedPoolUtxos);
                                                                                                            return Control_Bind.bind(Contract_Monad.bindContract)((function () {
                                                                                                                var $30 = Data_Foldable["null"](Data_Foldable.foldableArray)(depositList);
                                                                                                                if ($30) {
                                                                                                                    return Data_Traversable.traverse(Data_Traversable.traversableArray)(Contract_Monad.applicativeContract)(mkEntryUpdateList(v)(valHash))(assocList);
                                                                                                                };
                                                                                                                return Control_Bind.bind(Contract_Monad.bindContract)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Contract_Monad.applicativeContract)(mkEntryUpdateList(v)(valHash))(assocList))(function (constraintsLookupsList) {
                                                                                                                    return Contract_Monad.liftContractM("depositUnbondedPoolContract: Failed to create updateList'")(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(Data_Array.index(constraintsLookupsList))(depositList));
                                                                                                                });
                                                                                                            })())(function (updateList) {
                                                                                                                return Control_Bind.bind(Contract_Monad.bindContract)((function () {
                                                                                                                    var $31 = Data_Eq.eq(Types_Natural.eqNatural)(batchSize)(Data_Semiring.zero(Types_Natural.semiringNatural));
                                                                                                                    if ($31) {
                                                                                                                        return submitBatch(updateList);
                                                                                                                    };
                                                                                                                    var updateBatches = Utils.splitByLength(Utils.toIntUnsafe(batchSize))(updateList);
                                                                                                                    return Control_Bind.bind(Contract_Monad.bindContract)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Contract_Monad.applicativeContract)(submitBatch)(updateBatches))(function (failedDeposits$prime) {
                                                                                                                        return Control_Applicative.pure(Contract_Monad.applicativeContract)(Contract_Prelude.mconcat(Data_Foldable.foldableArray)(Data_Monoid.monoidArray)(failedDeposits$prime));
                                                                                                                    });
                                                                                                                })())(function (failedDeposits) {
                                                                                                                    return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Utils.logInfo_(Data_Show.showArray(Data_Tuple.showTuple(Types_TxConstraints.showTxConstraints(Data_Unit.showUnit)(Data_Unit.showUnit))(Types_ScriptLookups.showScriptLookups)))("depositUnbondedPoolContract: Finished updating pool entries. /Entries with failed updates")(failedDeposits))(function () {
                                                                                                                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("depositUnbondedPoolContract: Failed to create /failedDepositsIndicies list")(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(Data_Function.flip(Data_Array.elemIndex(Data_Tuple.eqTuple(Types_TxConstraints.eqTxConstraints(Data_Eq.eqUnit)(Data_Eq.eqUnit))(Types_ScriptLookups.eqScriptLookups)))(updateList))(failedDeposits)))(function (failedDepositsIndicies) {
                                                                                                                            return Control_Applicative.pure(Contract_Monad.applicativeContract)(failedDepositsIndicies);
                                                                                                                        });
                                                                                                                    });
                                                                                                                });
                                                                                                            });
                                                                                                        });
                                                                                                    };
                                                                                                    if (v2 instanceof UnbondedStaking_Types.StateDatum && (v2.value0.maybeEntryName instanceof Data_Maybe.Nothing && v2.value0.open)) {
                                                                                                        return Contract_Monad.throwContractError(Data_Show.showString)("depositUnbondedPoolContract: There are no users in the pool to deposit rewards for");
                                                                                                    };
                                                                                                    if (v2 instanceof UnbondedStaking_Types.StateDatum && !v2.value0.open) {
                                                                                                        return Contract_Monad.throwContractError(Data_Show.showString)("depositUnbondedPoolContract: Cannot deposit to a closed pool");
                                                                                                    };
                                                                                                    return Contract_Monad.throwContractError(Data_Show.showString)("depositUnbondedPoolContract: Datum incorrect type");
                                                                                                });
                                                                                            });
                                                                                        });
                                                                                    });
                                                                                });
                                                                            });
                                                                        });
                                                                    });
                                                                });
                                                            });
                                                        });
                                                    });
                                                });
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
    };
};
module.exports = {
    depositUnbondedPoolContract: depositUnbondedPoolContract
};
