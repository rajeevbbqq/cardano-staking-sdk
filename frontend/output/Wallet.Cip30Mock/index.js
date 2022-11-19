// Generated by purs version 0.14.5
"use strict";
var $foreign = require("./foreign.js");
var Cardano_Types_Transaction = require("../Cardano.Types.Transaction/index.js");
var Cardano_Types_Value = require("../Cardano.Types.Value/index.js");
var Contract_Monad = require("../Contract.Monad/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Control_Promise = require("../Control.Promise/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Lens_Common = require("../Data.Lens.Common/index.js");
var Data_Lens_Iso_Newtype = require("../Data.Lens.Iso.Newtype/index.js");
var Data_Lens_Record = require("../Data.Lens.Record/index.js");
var Data_Lens_Setter = require("../Data.Lens.Setter/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Profunctor = require("../Data.Profunctor/index.js");
var Data_Profunctor_Strong = require("../Data.Profunctor.Strong/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_UInt = require("../Data.UInt/index.js");
var Data_Unfoldable = require("../Data.Unfoldable/index.js");
var Deserialization_Transaction = require("../Deserialization.Transaction/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Unsafe = require("../Effect.Unsafe/index.js");
var Helpers = require("../Helpers/index.js");
var QueryM = require("../QueryM/index.js");
var QueryM_Utxos = require("../QueryM.Utxos/index.js");
var Serialization = require("../Serialization/index.js");
var Serialization_WitnessSet = require("../Serialization.WitnessSet/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Types_ByteArray = require("../Types.ByteArray/index.js");
var Types_CborBytes = require("../Types.CborBytes/index.js");
var Types_Transaction = require("../Types.Transaction/index.js");
var Untagged_Union = require("../Untagged.Union/index.js");
var Wallet = require("../Wallet/index.js");
var Wallet_Key = require("../Wallet.Key/index.js");
var MockFlint = (function () {
    function MockFlint() {

    };
    MockFlint.value = new MockFlint();
    return MockFlint;
})();
var MockGero = (function () {
    function MockGero() {

    };
    MockGero.value = new MockGero();
    return MockGero;
})();
var MockNami = (function () {
    function MockNami() {

    };
    MockNami.value = new MockNami();
    return MockNami;
})();
var mkCip30Mock = function (pKey) {
    return function (mSKey) {
        var keyWallet = Wallet_Key.privateKeysToKeyWallet(pKey)(mSKey);
        return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Control_Monad_Reader_Class.ask(QueryM.monadAskQueryEnvQueryMExt))(function (v) {
            var getCollateralUtxos = function (utxos) {
                var pparams = Data_Newtype.unwrap()(v.runtime.pparams);
                var maxCollateralInputs = Data_UInt.toInt(pparams.maxCollateralInputs);
                return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Control_Bind.bind(Effect.bindEffect)((Data_Newtype.unwrap()(keyWallet)).selectCollateral(pparams.coinsPerUtxoUnit)(maxCollateralInputs)(utxos))(Control_Monad_Error_Class.liftMaybe(Control_Monad_Error_Class.monadThrowEffect)(Effect_Exception.error("No UTxOs at address"))));
            };
            return Control_Applicative.pure(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))({
                getUsedAddresses: Control_Promise.fromAff(Data_Functor.mapFlipped(Effect_Aff.functorAff)((Data_Newtype.unwrap()(keyWallet)).address(v.config.networkId))(function (address) {
                    return [ Types_ByteArray.byteArrayToHex(Serialization.toBytes(Untagged_Union.asOneOf()(address))) ];
                })),
                getCollateral: Control_Promise.fromAff(Control_Bind.bind(Effect_Aff.bindAff)((Data_Newtype.unwrap()(keyWallet)).address(v.config.networkId))(function (ownAddress) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Bind.bindFlipped(Effect_Aff.bindAff)(Control_Monad_Error_Class.liftMaybe(Effect_Aff.monadThrowAff)(Effect_Exception.error("No UTxOs at address")))(QueryM.runQueryMInRuntime(v.config)(v.runtime)(QueryM_Utxos.utxosAt(ownAddress))))(function (utxos) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(getCollateralUtxos(utxos))(function (collateralUtxos) {
                            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect.applicativeEffect)(Serialization.convertTransactionUnspentOutput)(collateralUtxos)))(function (cslUnspentOutput) {
                                return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Functor.map(Data_Functor.functorArray)((function () {
                                    var $16 = Untagged_Union.asOneOf();
                                    return function ($17) {
                                        return Types_ByteArray.byteArrayToHex(Serialization.toBytes($16($17)));
                                    };
                                })())(cslUnspentOutput));
                            });
                        });
                    });
                })),
                signTx: function (str) {
                    return Effect_Unsafe.unsafePerformEffect(Control_Promise.fromAff(Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Error_Class.liftMaybe(Effect_Aff.monadThrowAff)(Effect_Exception.error("Unable to convert CBOR"))(Types_ByteArray.hexToByteArray(str)))(function (txBytes) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Control_Monad_Error_Class.liftMaybe(Effect_Aff.monadThrowAff)(Effect_Exception.error("Failed to decode Transaction CBOR"))(Data_Either.hush(Deserialization_Transaction.deserializeTransaction(Types_CborBytes.cborBytesFromByteArray(txBytes)))))(function (tx) {
                            return Control_Bind.bind(Effect_Aff.bindAff)((Data_Newtype.unwrap()(keyWallet)).signTx(tx))(function (witness) {
                                return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Serialization_WitnessSet.convertWitnessSet(witness)))(function (cslWitnessSet) {
                                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(Types_ByteArray.byteArrayToHex(Serialization.toBytes(Untagged_Union.asOneOf()(cslWitnessSet))));
                                });
                            });
                        });
                    })));
                },
                getBalance: Control_Promise.fromAff(Control_Bind.bind(Effect_Aff.bindAff)((Data_Newtype.unwrap()(keyWallet)).address(v.config.networkId))(function (ownAddress) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Bind.bindFlipped(Effect_Aff.bindAff)(Control_Monad_Error_Class.liftMaybe(Effect_Aff.monadThrowAff)(Effect_Exception.error("No UTxOs at address")))(QueryM.runQueryMInRuntime(v.config)(v.runtime)(QueryM_Utxos.utxosAt(ownAddress))))(function (utxos) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Serialization.convertValue(Data_Foldable.foldMap(Data_List_Types.foldableList)(Cardano_Types_Value.monoidValue)((function () {
                            var $18 = Data_Newtype.unwrap();
                            return function ($19) {
                                return (function (v1) {
                                    return v1.amount;
                                })($18($19));
                            };
                        })())(Data_Map_Internal.values(utxos)))))(function (value) {
                            return Control_Applicative.pure(Effect_Aff.applicativeAff)(Types_ByteArray.byteArrayToHex(Serialization.toBytes(Untagged_Union.asOneOf()(value))));
                        });
                    });
                })),
                getUtxos: Control_Promise.fromAff(Control_Bind.bind(Effect_Aff.bindAff)((Data_Newtype.unwrap()(keyWallet)).address(v.config.networkId))(function (ownAddress) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Bind.bindFlipped(Effect_Aff.bindAff)(Control_Monad_Error_Class.liftMaybe(Effect_Aff.monadThrowAff)(Effect_Exception.error("No UTxOs at address")))(QueryM.runQueryMInRuntime(v.config)(v.runtime)(QueryM_Utxos.utxosAt(ownAddress))))(function (utxos) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(getCollateralUtxos(utxos))(function (collateralUtxos) {
                            var nonCollateralUtxos = Data_Map_Internal.filter(Types_Transaction.ordTransactionInput)(Data_Function.flip(Data_Array.elem(Cardano_Types_Transaction.eqTransactionOutput))(Data_Functor.mapFlipped(Data_Functor.functorArray)(collateralUtxos)((function () {
                                var $20 = Data_Newtype.unwrap();
                                return function ($21) {
                                    return (function (v1) {
                                        return v1.output;
                                    })($20($21));
                                };
                            })())))(utxos);
                            return Control_Bind.bind(Effect_Aff.bindAff)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Effect_Aff.applicativeAff)((function () {
                                var $22 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
                                return function ($23) {
                                    return $22(Serialization.convertTransactionUnspentOutput($23));
                                };
                            })())(Data_Functor.mapFlipped(Data_Functor.functorArray)(Data_Map_Internal.toUnfoldable(Data_Unfoldable.unfoldableArray)(nonCollateralUtxos))(function (v1) {
                                return {
                                    input: v1.value0,
                                    output: v1.value1
                                };
                            })))(function (cslUtxos) {
                                return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Functor.map(Data_Functor.functorArray)((function () {
                                    var $24 = Untagged_Union.asOneOf();
                                    return function ($25) {
                                        return Types_ByteArray.byteArrayToHex(Serialization.toBytes($24($25)));
                                    };
                                })())(cslUtxos));
                            });
                        });
                    });
                }))
            });
        });
    };
};
var withCip30Mock = function (v) {
    return function (mock) {
        return function (contract) {
            var mockString = (function () {
                if (mock instanceof MockFlint) {
                    return "flint";
                };
                if (mock instanceof MockGero) {
                    return "gerowallet";
                };
                if (mock instanceof MockNami) {
                    return "nami";
                };
                throw new Error("Failed pattern match at Wallet.Cip30Mock (line 93, column 16 - line 96, column 23): " + [ mock.constructor.name ]);
            })();
            var mkWalletAff = (function () {
                if (mock instanceof MockFlint) {
                    return Wallet.mkFlintWalletAff;
                };
                if (mock instanceof MockGero) {
                    return Wallet.mkGeroWalletAff;
                };
                if (mock instanceof MockNami) {
                    return Wallet.mkNamiWalletAff;
                };
                throw new Error("Failed pattern match at Wallet.Cip30Mock (line 87, column 17 - line 90, column 32): " + [ mock.constructor.name ]);
            })();
            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.wrapContract(mkCip30Mock(v.paymentKey)(v.stakeKey)))(function (cip30Mock) {
                return Control_Bind.bind(Contract_Monad.bindContract)(Effect_Class.liftEffect(Contract_Monad.monadEffectContract)($foreign.injectCip30Mock(mockString)(cip30Mock)))(function (deleteMock) {
                    return Control_Bind.bind(Contract_Monad.bindContract)(Effect_Aff_Class.liftAff(Contract_Monad.monadAffContract)(mkWalletAff))(function (wallet) {
                        var setUpdatedWallet = Data_Lens_Setter.set((function () {
                            var $26 = Data_Lens_Common.simple(Data_Lens_Iso_Newtype["_Newtype"]()()(Data_Profunctor.profunctorFn));
                            var $27 = Data_Lens_Record.prop({
                                reflectSymbol: function () {
                                    return "runtime";
                                }
                            })()()(Type_Proxy["Proxy"].value)(Data_Profunctor_Strong.strongFn);
                            var $28 = Data_Lens_Record.prop({
                                reflectSymbol: function () {
                                    return "wallet";
                                }
                            })()()(Type_Proxy["Proxy"].value)(Data_Profunctor_Strong.strongFn);
                            return function ($29) {
                                return $26($27($28($29)));
                            };
                        })())(new Data_Maybe.Just(wallet));
                        return Control_Bind.bind(Contract_Monad.bindContract)(Control_Monad_Error_Class["try"](Contract_Monad.monadErrorErrorContract)(Control_Monad_Reader_Class.local(Contract_Monad.monadReaderContractEnvCon)(setUpdatedWallet)(contract)))(function (res) {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Effect_Class.liftEffect(Contract_Monad.monadEffectContract)(deleteMock))(function () {
                                return Helpers.liftEither(Contract_Monad.monadErrorErrorContract)(res);
                            });
                        });
                    });
                });
            });
        };
    };
};
module.exports = {
    MockFlint: MockFlint,
    MockGero: MockGero,
    MockNami: MockNami,
    withCip30Mock: withCip30Mock,
    mkCip30Mock: mkCip30Mock,
    injectCip30Mock: $foreign.injectCip30Mock
};
