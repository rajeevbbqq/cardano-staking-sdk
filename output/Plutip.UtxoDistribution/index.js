// Generated by purs version 0.14.5
"use strict";
var Contract_Address = require("../Contract.Address/index.js");
var Contract_Monad = require("../Contract.Monad/index.js");
var Contract_ScriptLookups = require("../Contract.ScriptLookups/index.js");
var Contract_Transaction = require("../Contract.Transaction/index.js");
var Contract_Utxos = require("../Contract.Utxos/index.js");
var Contract_Wallet = require("../Contract.Wallet/index.js");
var Control_Alternative = require("../Control.Alternative/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_FoldableWithIndex = require("../Data.FoldableWithIndex/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_List_Types = require("../Data.List.Types/index.js");
var Data_Map_Internal = require("../Data.Map.Internal/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Ref = require("../Effect.Ref/index.js");
var FromData = require("../FromData/index.js");
var IsData = require("../IsData/index.js");
var Plutip_Types = require("../Plutip.Types/index.js");
var ToData = require("../ToData/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Types_ScriptLookups = require("../Types.ScriptLookups/index.js");
var Types_TxConstraints = require("../Types.TxConstraints/index.js");
var Wallet_Key = require("../Wallet.Key/index.js");
var withStakeKey = Plutip_Types.InitialUTxOsWithStakeKey.create;
var transferFundsFromEnterpriseToBase = function (ourKey) {
    return function (wallets) {
        var constraintsForWallet = function (v) {
            return Data_Semigroup.append(Types_TxConstraints.semigroupTxConstraints)(Types_TxConstraints.mustBeSignedBy(v.payPkh))(Data_FoldableWithIndex.foldMapWithIndex(Data_Map_Internal.foldableWithIndexMap)(Types_TxConstraints.monoidTxConstraints)(function (input) {
                return function (v1) {
                    return Data_Semigroup.append(Types_TxConstraints.semigroupTxConstraints)(Types_TxConstraints.mustPayToPubKeyAddress(v.payPkh)(v.stakePkh)((Data_Newtype.unwrap()(v1.output)).amount))(Types_TxConstraints.mustSpendPubKeyOutput(input));
                };
            })(v.utxos));
        };
        var addStakeKeyWalletInfo = function (walletsInfo) {
            return function (wallet) {
                return Contract_Wallet.withKeyWallet(wallet)(Control_Bind.bind(Contract_Monad.bindContract)(Contract_Address.ownStakePubKeyHash)(function (v) {
                    if (v instanceof Data_Maybe.Nothing) {
                        return Control_Applicative.pure(Contract_Monad.applicativeContract)(walletsInfo);
                    };
                    if (v instanceof Data_Maybe.Just) {
                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("Could not get payment pubkeyhash")(Contract_Address.ownPaymentPubKeyHash))(function (payPkh) {
                            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Address.getNetworkId)(function (networkId) {
                                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("Could not get wallet address")(Contract_Address.payPubKeyHashEnterpriseAddress(networkId)(payPkh)))(function (addr) {
                                    return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("Could not find utxos")(Contract_Utxos.utxosAt(addr)))(function (utxos$prime) {
                                        return Control_Applicative.pure(Contract_Monad.applicativeContract)(new Data_List_Types.Cons({
                                            utxos: utxos$prime,
                                            payPkh: payPkh,
                                            stakePkh: v.value0,
                                            wallet: wallet
                                        }, walletsInfo));
                                    });
                                });
                            });
                        });
                    };
                    throw new Error("Failed pattern match at Plutip.UtxoDistribution (line 230, column 28 - line 239, column 73): " + [ v.constructor.name ]);
                }));
            };
        };
        return Control_Bind.bind(Contract_Monad.bindContract)(Data_Foldable.foldM(Data_Foldable.foldableArray)(Contract_Monad.monadContract)(addStakeKeyWalletInfo)(Data_Monoid.mempty(Data_List_Types.monoidList))(wallets))(function (walletsInfo) {
            return Control_Applicative.unless(Contract_Monad.applicativeContract)(Data_Foldable["null"](Data_List_Types.foldableList)(walletsInfo))((function () {
                var ourWallet = Wallet_Key.privateKeysToKeyWallet(ourKey)(Data_Maybe.Nothing.value);
                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("Could not get our address")(Contract_Wallet.withKeyWallet(ourWallet)(Contract_Address.getWalletAddress)))(function (ourAddr) {
                    return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("Could not find our utxos")(Contract_Utxos.utxosAt(ourAddr)))(function (ourUtxos) {
                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedM("Could not get our payment pkh")(Contract_Wallet.withKeyWallet(ourWallet)(Contract_Address.ownPaymentPubKeyHash)))(function (ourPkh) {
                            var lookups = Data_Semigroup.append(Types_ScriptLookups.semigroupScriptLookups)(Types_ScriptLookups.unspentOutputs(ourUtxos))(Data_Foldable.foldMap(Data_List_Types.foldableList)(Types_ScriptLookups.monoidScriptLookups)(function ($96) {
                                return Types_ScriptLookups.unspentOutputs((function (v) {
                                    return v.utxos;
                                })($96));
                            })(walletsInfo));
                            var constraints = Data_Semigroup.append(Types_TxConstraints.semigroupTxConstraints)(Types_TxConstraints.mustBeSignedBy(ourPkh))(Data_Foldable.foldMap(Data_List_Types.foldableList)(Types_TxConstraints.monoidTxConstraints)(constraintsForWallet)(walletsInfo));
                            return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedE(Types_ScriptLookups.showMkUnbalancedTxError)(Contract_ScriptLookups.mkUnbalancedTx()(IsData.isData(FromData.fromDataVoid)(ToData.toDataVoid))(IsData.isData(FromData.fromDataVoid)(ToData.toDataVoid))(lookups)(constraints)))(function (unbalancedTx) {
                                return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftedE(Effect_Exception.showError)(Contract_Wallet.withKeyWallet(ourWallet)(Contract_Transaction.balanceAndSignTxE(unbalancedTx))))(function (signedTx) {
                                    return Control_Bind.bind(Contract_Monad.bindContract)(Data_Foldable.foldM(Data_List_Types.foldableList)(Contract_Monad.monadContract)(function (tx) {
                                        return function (v) {
                                            return Contract_Monad.liftedM("Could not sign")(Contract_Wallet.withKeyWallet(v.wallet)(Contract_Transaction.signTransaction(tx)));
                                        };
                                    })(Data_Newtype.unwrap()(signedTx))(walletsInfo))(function (signedTx$prime) {
                                        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Transaction.submit(Data_Newtype.wrap()(signedTx$prime)))(function (txHash) {
                                            return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Contract_Transaction.awaitTxConfirmed(txHash))(function () {
                                                return Control_Bind.bind(Contract_Monad.bindContract)(Control_Monad_Reader_Class.asks(Contract_Monad.monadAskContractEnvContra)((function () {
                                                    var $97 = Data_Newtype.unwrap();
                                                    var $98 = Data_Newtype.unwrap();
                                                    return function ($99) {
                                                        return $97((function (v) {
                                                            return v.usedTxOuts;
                                                        })((function (v) {
                                                            return v.runtime;
                                                        })($98($99))));
                                                    };
                                                })()))(function (cache) {
                                                    return Effect_Class.liftEffect(Contract_Monad.monadEffectContract)(Effect_Ref.write(Data_Map_Internal.empty)(cache));
                                                });
                                            });
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            })());
        });
    };
};
var keyWallets = function (dict) {
    return dict.keyWallets;
};
var encodeDistribution = function (dict) {
    return dict.encodeDistribution;
};
var decodeWallets$prime = function (dict) {
    return dict["decodeWallets'"];
};
var decodeWalletsDefault = function (dictUtxoDistribution) {
    return function (d) {
        return function (p) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(decodeWallets$prime(dictUtxoDistribution)(d)(p))(function (v) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Data_Maybe.bindMaybe)(Control_Alternative.guard(Data_Maybe.alternativeMaybe)(Data_Array["null"](v.value1)))(function () {
                    return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(v.value0);
                });
            });
        };
    };
};
var utxoDistributionInitialUT = {
    encodeDistribution: function (amounts) {
        return [ amounts ];
    },
    decodeWallets: function (d) {
        return function (p) {
            return decodeWalletsDefault(utxoDistributionInitialUT)(d)(p);
        };
    },
    "decodeWallets'": function (v) {
        return function (pks) {
            return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Array.uncons(pks))(function (v1) {
                return new Data_Tuple.Tuple(Wallet_Key.privateKeysToKeyWallet(v1.head)(Data_Maybe.Nothing.value), v1.tail);
            });
        };
    },
    keyWallets: function (v) {
        return function (wallet) {
            return [ wallet ];
        };
    }
};
var utxoDistributionInitialUT1 = {
    encodeDistribution: function (v) {
        return [ v.value1 ];
    },
    decodeWallets: function (d) {
        return function (p) {
            return decodeWalletsDefault(utxoDistributionInitialUT1)(d)(p);
        };
    },
    "decodeWallets'": function (v) {
        return function (pks) {
            return Data_Functor.mapFlipped(Data_Maybe.functorMaybe)(Data_Array.uncons(pks))(function (v1) {
                return new Data_Tuple.Tuple(Wallet_Key.privateKeysToKeyWallet(v1.head)(new Data_Maybe.Just(v.value0)), v1.tail);
            });
        };
    },
    keyWallets: function (v) {
        return function (wallet) {
            return [ wallet ];
        };
    }
};
var utxoDistributionUnitUnit = {
    encodeDistribution: function (v) {
        return [  ];
    },
    decodeWallets: function (d) {
        return function (p) {
            return decodeWalletsDefault(utxoDistributionUnitUnit)(d)(p);
        };
    },
    "decodeWallets'": function (v) {
        return function (pks) {
            return Data_Maybe.Just.create(new Data_Tuple.Tuple(Data_Unit.unit, pks));
        };
    },
    keyWallets: function (v) {
        return function (v1) {
            return [  ];
        };
    }
};
var utxoDistribution$div$bslash$div$bslash = function (dictUtxoDistribution) {
    return function (dictUtxoDistribution1) {
        return {
            encodeDistribution: function (v) {
                return Data_Semigroup.append(Data_Semigroup.semigroupArray)(encodeDistribution(dictUtxoDistribution)(v.value0))(encodeDistribution(dictUtxoDistribution1)(v.value1));
            },
            decodeWallets: function (d) {
                return function (p) {
                    return decodeWalletsDefault(utxoDistribution$div$bslash$div$bslash(dictUtxoDistribution)(dictUtxoDistribution1))(d)(p);
                };
            },
            "decodeWallets'": function (v) {
                return function (pks) {
                    return Control_Bind.bind(Data_Maybe.bindMaybe)(decodeWallets$prime(dictUtxoDistribution)(v.value0)(pks))(function (v1) {
                        return Control_Bind.bind(Data_Maybe.bindMaybe)(decodeWallets$prime(dictUtxoDistribution1)(v.value1)(v1.value1))(function (v2) {
                            return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(new Data_Tuple.Tuple(v1.value0, v2.value0), v2.value1));
                        });
                    });
                };
            },
            keyWallets: function (v) {
                return function (v1) {
                    return Data_Semigroup.append(Data_Semigroup.semigroupArray)(keyWallets(dictUtxoDistribution)(Type_Proxy["Proxy"].value)(v1.value0))(keyWallets(dictUtxoDistribution1)(Type_Proxy["Proxy"].value)(v1.value1));
                };
            }
        };
    };
};
var decodeWallets = function (dict) {
    return dict.decodeWallets;
};
var utxoDistributionArrayInit = {
    encodeDistribution: function (amounts) {
        return amounts;
    },
    decodeWallets: function (v) {
        return function (privateKeyResponses) {
            return Data_Maybe.Just.create(Data_Functor.map(Data_Functor.functorArray)(function (v1) {
                return Wallet_Key.privateKeysToKeyWallet(v1)(Data_Maybe.Nothing.value);
            })(privateKeyResponses));
        };
    },
    "decodeWallets'": function (listOfInitialUTxOs) {
        return function (privateKeyResponses) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function (utxos) {
                return decodeWallets(utxoDistributionInitialUT)(utxos)(privateKeyResponses);
            })(listOfInitialUTxOs))(function (wallets) {
                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(wallets, privateKeyResponses));
            });
        };
    },
    keyWallets: function (v) {
        return function (wallets) {
            return wallets;
        };
    }
};
var utxoDistributionArrayInit1 = {
    encodeDistribution: function (listOfInitialUTxOsWithStakeKey) {
        return Data_Functor.map(Data_Functor.functorArray)(function (v) {
            return v.value1;
        })(listOfInitialUTxOsWithStakeKey);
    },
    decodeWallets: function (listOfInitialUTxOsWithStakeKey) {
        return function (privateKeyResponses) {
            return Data_Maybe.Just.create(Data_Functor.map(Data_Functor.functorArray)(function (v) {
                return Wallet_Key.privateKeysToKeyWallet(v.value0)(new Data_Maybe.Just(v.value1.value0));
            })(Data_Array.zip(privateKeyResponses)(listOfInitialUTxOsWithStakeKey)));
        };
    },
    "decodeWallets'": function (listOfInitialUTxOsWithStakeKey) {
        return function (privateKeyResponses) {
            return Control_Bind.bind(Data_Maybe.bindMaybe)(Data_Traversable.traverse(Data_Traversable.traversableArray)(Data_Maybe.applicativeMaybe)(function (utxos) {
                return decodeWallets(utxoDistributionInitialUT1)(utxos)(privateKeyResponses);
            })(listOfInitialUTxOsWithStakeKey))(function (wallets) {
                return Control_Applicative.pure(Data_Maybe.applicativeMaybe)(new Data_Tuple.Tuple(wallets, privateKeyResponses));
            });
        };
    },
    keyWallets: function (v) {
        return function (wallets) {
            return wallets;
        };
    }
};
module.exports = {
    decodeWallets: decodeWallets,
    "decodeWallets'": decodeWallets$prime,
    keyWallets: keyWallets,
    encodeDistribution: encodeDistribution,
    transferFundsFromEnterpriseToBase: transferFundsFromEnterpriseToBase,
    withStakeKey: withStakeKey,
    utxoDistributionUnitUnit: utxoDistributionUnitUnit,
    utxoDistributionInitialUT: utxoDistributionInitialUT,
    utxoDistributionInitialUT1: utxoDistributionInitialUT1,
    utxoDistributionArrayInit: utxoDistributionArrayInit,
    utxoDistributionArrayInit1: utxoDistributionArrayInit1,
    "utxoDistribution/\\/\\": utxoDistribution$div$bslash$div$bslash
};
