// Generated by purs version 0.14.5
"use strict";
var Aeson = require("../Aeson/index.js");
var Affjax = require("../Affjax/index.js");
var Affjax_RequestBody = require("../Affjax.RequestBody/index.js");
var Affjax_RequestHeader = require("../Affjax.RequestHeader/index.js");
var Affjax_ResponseFormat = require("../Affjax.ResponseFormat/index.js");
var Contract_Monad = require("../Contract.Monad/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_HTTP_Method = require("../Data.HTTP.Method/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Monoid = require("../Data.Monoid/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Posix_Signal = require("../Data.Posix.Signal/index.js");
var Data_Semigroup = require("../Data.Semigroup/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_String_CodeUnits = require("../Data.String.CodeUnits/index.js");
var Data_Time_Duration = require("../Data.Time.Duration/index.js");
var Data_Traversable = require("../Data.Traversable/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Data_UInt = require("../Data.UInt/index.js");
var Data_Unit = require("../Data.Unit/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Aff_Retry = require("../Effect.Aff.Retry/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Node_ChildProcess = require("../Node.ChildProcess/index.js");
var Plutip_PortCheck = require("../Plutip.PortCheck/index.js");
var Plutip_Spawn = require("../Plutip.Spawn/index.js");
var Plutip_Types = require("../Plutip.Types/index.js");
var Plutip_Utils = require("../Plutip.Utils/index.js");
var Plutip_UtxoDistribution = require("../Plutip.UtxoDistribution/index.js");
var QueryM = require("../QueryM/index.js");
var QueryM_Logging = require("../QueryM.Logging/index.js");
var QueryM_ServerConfig = require("../QueryM.ServerConfig/index.js");
var QueryM_UniqueId = require("../QueryM.UniqueId/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var Type_Proxy = require("../Type.Proxy/index.js");
var Types_UsedTxOuts = require("../Types.UsedTxOuts/index.js");
var stopChildProcess = (function () {
    var $62 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
    var $63 = Node_ChildProcess.kill(Data_Posix_Signal.SIGINT.value);
    return function ($64) {
        return $62($63($64));
    };
})();
var startOgmiosDatumCache = function (cfg) {
    return function (_params) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(QueryM_UniqueId.uniqueId("token")))(function (apiKey) {
            var $$arguments = [ "--server-api", apiKey, "--server-port", Data_UInt.toString(cfg.ogmiosDatumCacheConfig.port), "--ogmios-address", cfg.ogmiosDatumCacheConfig.host, "--ogmios-port", Data_UInt.toString(cfg.ogmiosConfig.port), "--db-port", Data_UInt.toString(cfg.postgresConfig.port), "--db-host", cfg.postgresConfig.host, "--db-user", cfg.postgresConfig.user, "--db-name", cfg.postgresConfig.dbname, "--db-password", cfg.postgresConfig.password, "--use-latest", "--from-origin" ];
            return Control_Bind.bind(Effect_Aff.bindAff)(Plutip_Spawn.spawnAndWaitForOutput("ogmios-datum-cache")($$arguments)(Node_ChildProcess.defaultSpawnOptions)((function () {
                var $65 = Data_Maybe.maybe(Plutip_Spawn.NoOp.value)(Data_Function["const"](Plutip_Spawn.Success.value));
                var $66 = Data_String_CodeUnits.indexOf("Intersection found");
                return function ($67) {
                    return $65($66($67));
                };
            })()))(function (child) {
                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Plutip_Spawn.killOnExit(child)))(function () {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(child);
                });
            });
        });
    };
};
var startOgmios = function (cfg) {
    return function (params) {
        var ogmiosArgs = [ "--host", cfg.ogmiosConfig.host, "--port", Data_UInt.toString(cfg.ogmiosConfig.port), "--node-socket", params.nodeSocketPath, "--node-config", params.nodeConfigPath ];
        return Control_Bind.bind(Effect_Aff.bindAff)(Plutip_Spawn.spawnAndWaitForOutput("ogmios")(ogmiosArgs)(Node_ChildProcess.defaultSpawnOptions)(Control_Applicative.pure(Control_Applicative.applicativeFn)(Plutip_Spawn.Success.value)))(function (child) {
            return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Plutip_Spawn.killOnExit(child)))(function () {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(child);
            });
        });
    };
};
var startCtlServer = function (serverPort) {
    var ctlServerArgs = [ "--port", Data_UInt.toString(serverPort) ];
    return Control_Bind.bind(Effect_Aff.bindAff)(Plutip_Spawn.spawnAndWaitForOutput("ctl-server")(ctlServerArgs)(Node_ChildProcess.defaultSpawnOptions)((function () {
        var $68 = Data_Maybe.maybe(Plutip_Spawn.NoOp.value)(Data_Function["const"](Plutip_Spawn.Success.value));
        var $69 = Data_String_CodeUnits.indexOf("CTL server starting on port");
        return function ($70) {
            return $68($69($70));
        };
    })()))(function (child) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Plutip_Spawn.killOnExit(child)))(function () {
            return Control_Applicative.pure(Effect_Aff.applicativeAff)(child);
        });
    });
};
var ourInitialUtxos = function (utxoDistribution) {
    var total = Data_Array.foldr((function () {
        var $71 = Data_Semiring.add(Data_BigInt.semiringBigInt);
        var $72 = Data_Foldable.sum(Data_Foldable.foldableArray)(Data_BigInt.semiringBigInt);
        return function ($73) {
            return $71($72($73));
        };
    })())(Data_Semiring.zero(Data_BigInt.semiringBigInt))(utxoDistribution);
    return [ Data_Semiring.add(Data_BigInt.semiringBigInt)(total)(Data_BigInt.fromInt(1000000000)) ];
};
var mkServerEndpointUrl = function (cfg) {
    return function (path) {
        return "http://" + (cfg.host + (":" + (Data_UInt.toString(cfg.port) + ("/" + path))));
    };
};
var startPlutipCluster = function (cfg) {
    return function (keysToGenerate) {
        var url = mkServerEndpointUrl(cfg)("start");
        return Control_Bind.bind(Effect_Aff.bindAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_Class.liftAff(Effect_Aff_Class.monadAffAff)(Affjax.request({
            method: new Data_Either.Left(Data_HTTP_Method.POST.value),
            url: url,
            headers: [ new Affjax_RequestHeader.ContentType(Data_Newtype.wrap()("application/json")) ],
            content: Data_Maybe.Just.create(Affjax_RequestBody["String"].create(Aeson.stringifyAeson(Aeson.encodeAeson(Plutip_Types.encodeAesonClusterStartup)({
                keysToGenerate: keysToGenerate
            })))),
            username: Affjax.defaultRequest.username,
            password: Affjax.defaultRequest.password,
            withCredentials: Affjax.defaultRequest.withCredentials,
            responseFormat: Affjax_ResponseFormat.string,
            timeout: Affjax.defaultRequest.timeout
        })))(function (response) {
            return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Either.either(function ($74) {
                return Data_Either.Left.create(QueryM.ClientHttpError.create($74));
            })((function () {
                var $75 = Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither)(QueryM.ClientDecodeJsonError.create);
                var $76 = Control_Bind.composeKleisliFlipped(Data_Either.bindEither)(Aeson.decodeAeson(Plutip_Types.decodeAesonStartClusterRe))(Aeson.parseJsonStringToAeson);
                return function ($77) {
                    return $75($76((function (v) {
                        return v.body;
                    })($77)));
                };
            })())(response));
        }))(function (res) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Data_Either.either((function () {
                var $78 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
                var $79 = Data_Show.show(QueryM.showClientError);
                return function ($80) {
                    return $78(Effect_Exception["throw"]($79($80)));
                };
            })())(Control_Applicative.pure(Effect_Aff.applicativeAff))(res))(function (v) {
                if (v instanceof Plutip_Types.ClusterStartupFailure) {
                    return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception["throw"]("Failed to start up cluster"));
                };
                if (v instanceof Plutip_Types.ClusterStartupSuccess) {
                    var v1 = Data_Array.uncons(v.value0.privateKeys);
                    if (v1 instanceof Data_Maybe.Nothing) {
                        return Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception["throw"]("Impossible happened: insufficient private keys provided by plutip. Please report as bug."));
                    };
                    if (v1 instanceof Data_Maybe.Just) {
                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(new Data_Tuple.Tuple(v1.value0.head, {
                            privateKeys: v1.value0.tail,
                            keysDirectory: v.value0.keysDirectory,
                            nodeConfigPath: v.value0.nodeConfigPath,
                            nodeSocketPath: v.value0.nodeSocketPath
                        }));
                    };
                    throw new Error("Failed pattern match at Plutip.Server (line 290, column 9 - line 295, column 79): " + [ v1.constructor.name ]);
                };
                throw new Error("Failed pattern match at Plutip.Server (line 286, column 5 - line 295, column 79): " + [ v.constructor.name ]);
            });
        });
    };
};
var stopPlutipCluster = function (cfg) {
    var url = mkServerEndpointUrl(cfg)("stop");
    return Control_Bind.bind(Effect_Aff.bindAff)(Control_Bind.bind(Effect_Aff.bindAff)(Effect_Aff_Class.liftAff(Effect_Aff_Class.monadAffAff)(Affjax.request({
        method: new Data_Either.Left(Data_HTTP_Method.POST.value),
        url: url,
        headers: [ new Affjax_RequestHeader.ContentType(Data_Newtype.wrap()("application/json")) ],
        content: Data_Maybe.Just.create(Affjax_RequestBody["String"].create(Aeson.stringifyAeson(Aeson.encodeAeson(Plutip_Types.encodeAesonStopClusterReq)(Plutip_Types.StopClusterRequest.value)))),
        username: Affjax.defaultRequest.username,
        password: Affjax.defaultRequest.password,
        withCredentials: Affjax.defaultRequest.withCredentials,
        responseFormat: Affjax_ResponseFormat.string,
        timeout: Affjax.defaultRequest.timeout
    })))(function (response) {
        return Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Either.either(function ($81) {
            return Data_Either.Left.create(QueryM.ClientHttpError.create($81));
        })((function () {
            var $82 = Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither)(QueryM.ClientDecodeJsonError.create);
            var $83 = Control_Bind.composeKleisliFlipped(Data_Either.bindEither)(Aeson.decodeAeson(Plutip_Types.decodeAesonStopClusterRes))(Aeson.parseJsonStringToAeson);
            return function ($84) {
                return $82($83((function (v) {
                    return v.body;
                })($84)));
            };
        })())(response));
    }))(function (res) {
        return Data_Either.either((function () {
            var $85 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
            var $86 = Data_Show.show(QueryM.showClientError);
            return function ($87) {
                return $85(Effect_Exception["throw"]($86($87)));
            };
        })())(Control_Applicative.pure(Effect_Aff.applicativeAff))(res);
    });
};
var mkClusterContractEnv = function (plutipCfg) {
    return function (logger) {
        return function (customLogger) {
            return Control_Bind.bind(Effect_Aff.bindAff)(QueryM.mkDatumCacheWebSocketAff(logger)({
                port: plutipCfg.ogmiosDatumCacheConfig.port,
                host: plutipCfg.ogmiosDatumCacheConfig.host,
                secure: QueryM_ServerConfig.defaultDatumCacheWsConfig.secure,
                path: QueryM_ServerConfig.defaultDatumCacheWsConfig.path
            }))(function (datumCacheWs) {
                return Control_Bind.bind(Effect_Aff.bindAff)(QueryM.mkOgmiosWebSocketAff(datumCacheWs)(logger)({
                    port: plutipCfg.ogmiosConfig.port,
                    host: plutipCfg.ogmiosConfig.host,
                    secure: QueryM_ServerConfig.defaultOgmiosWsConfig.secure,
                    path: QueryM_ServerConfig.defaultOgmiosWsConfig.path
                }))(function (ogmiosWs) {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Types_UsedTxOuts.newUsedTxOuts(Effect_Aff.monadEffectAff))(function (usedTxOuts) {
                        return Control_Bind.bind(Effect_Aff.bindAff)(QueryM.getProtocolParametersAff(ogmiosWs)(logger))(function (pparams) {
                            return Control_Applicative.pure(Effect_Aff.applicativeAff)({
                                config: {
                                    ctlServerConfig: plutipCfg.ctlServerConfig,
                                    ogmiosConfig: plutipCfg.ogmiosConfig,
                                    datumCacheConfig: plutipCfg.ogmiosDatumCacheConfig,
                                    networkId: Serialization_Address.MainnetId.value,
                                    logLevel: plutipCfg.logLevel,
                                    walletSpec: Data_Maybe.Nothing.value,
                                    customLogger: customLogger,
                                    suppressLogs: plutipCfg.suppressLogs
                                },
                                runtime: {
                                    ogmiosWs: ogmiosWs,
                                    datumCacheWs: datumCacheWs,
                                    wallet: Data_Maybe.Nothing.value,
                                    usedTxOuts: usedTxOuts,
                                    pparams: pparams
                                },
                                extraConfig: {}
                            });
                        });
                    });
                });
            });
        };
    };
};
var defaultRetryPolicy = function (dictMonadAff) {
    return Effect_Aff_Retry.limitRetriesByCumulativeDelay((dictMonadAff.MonadEffect0()).Monad0())(Data_Time_Duration.durationMilliseconds)(3000.0)(Effect_Aff_Retry.constantDelay(Data_Time_Duration.durationMilliseconds)(100.0)(dictMonadAff));
};
var startPlutipServer = function (cfg) {
    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Node_ChildProcess.spawn("plutip-server")([ "-p", Data_UInt.toString(cfg.port) ])(Node_ChildProcess.defaultSpawnOptions)))(function (p) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Plutip_Spawn.killOnExit(p)))(function () {
            return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Functor["void"](Effect_Aff.functorAff)(Effect_Aff_Retry.recovering(Effect_Aff_Class.monadAffAff)(Effect_Aff.monadErrorAff)(defaultRetryPolicy(Effect_Aff_Class.monadAffAff))([ function (v) {
                return function (v1) {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(true);
                };
            } ])(Data_Function["const"](stopPlutipCluster(cfg)))))(function () {
                return Control_Applicative.pure(Effect_Aff.applicativeAff)(p);
            });
        });
    });
};
var startPostgresServer = function (pgConfig) {
    return function (v) {
        return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Plutip_Utils.tmpdir))(function (tmpDir) {
            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(QueryM_UniqueId.uniqueId("")))(function (randomStr) {
                var workingDir = tmpDir + ("/" + randomStr);
                var postgresSocket = workingDir + "/postgres";
                var databaseDir = workingDir + "/postgres/data";
                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Functor["void"](Effect.functorEffect)(Node_ChildProcess.execSync("initdb " + databaseDir)(Node_ChildProcess.defaultExecSyncOptions))))(function () {
                    return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Node_ChildProcess.spawn("postgres")([ "-D", databaseDir, "-p", Data_UInt.toString(pgConfig.port), "-h", pgConfig.host, "-k", postgresSocket ])(Node_ChildProcess.defaultSpawnOptions)))(function (pgChildProcess) {
                        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Plutip_Spawn.killOnExit(pgChildProcess)))(function () {
                            return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Functor["void"](Effect_Aff.functorAff)(Effect_Aff_Retry.recovering(Effect_Aff_Class.monadAffAff)(Effect_Aff.monadErrorAff)(defaultRetryPolicy(Effect_Aff_Class.monadAffAff))([ function (v1) {
                                return function (v2) {
                                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(true);
                                };
                            } ])(Data_Function["const"](Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Node_ChildProcess.execSync("psql -h " + (pgConfig.host + (" -p " + (Data_UInt.toString(pgConfig.port) + " -d postgres"))))(Node_ChildProcess.defaultExecSyncOptions))))))(function () {
                                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Functor["void"](Effect.functorEffect)(Node_ChildProcess.execSync("psql -h " + (pgConfig.host + (" -p " + (Data_UInt.toString(pgConfig.port) + (" -d postgres" + (" -c \"CREATE ROLE " + (pgConfig.user + (" WITH LOGIN SUPERUSER CREATEDB PASSWORD '" + (pgConfig.password + "';\"")))))))))(Node_ChildProcess.defaultExecSyncOptions))))(function () {
                                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Data_Functor["void"](Effect.functorEffect)(Node_ChildProcess.execSync("createdb -h " + (pgConfig.host + (" -p " + (Data_UInt.toString(pgConfig.port) + (" -U " + (pgConfig.user + (" -O " + (pgConfig.user + (" " + pgConfig.dbname)))))))))(Node_ChildProcess.defaultExecSyncOptions))))(function () {
                                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(pgChildProcess);
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
var stopChildProcessWithPort = function (port) {
    return function (childProcess) {
        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(stopChildProcess(childProcess))(function () {
            return Data_Functor["void"](Effect_Aff.functorAff)(Effect_Aff_Retry.recovering(Effect_Aff_Class.monadAffAff)(Effect_Aff.monadErrorAff)(defaultRetryPolicy(Effect_Aff_Class.monadAffAff))([ function (v) {
                return function (v1) {
                    return Control_Applicative.pure(Effect_Aff.applicativeAff)(true);
                };
            } ])(function (v) {
                return Control_Bind.bind(Effect_Aff.bindAff)(Plutip_PortCheck.isPortAvailable(port))(function (isAvailable) {
                    return Control_Applicative.unless(Effect_Aff.applicativeAff)(isAvailable)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception["throw"]("retry")));
                });
            }));
        });
    };
};
var configCheck = function (cfg) {
    var printServiceEntry = function (v) {
        return "- " + (v.value1 + (" (port: " + (Data_Show.show(Data_Show.showInt)(Data_UInt.toInt(v.value0)) + ")\x0a")));
    };
    var services = Data_Semigroup.append(Data_Semigroup.semigroupArray)([ new Data_Tuple.Tuple(cfg.port, "plutip-server"), new Data_Tuple.Tuple(cfg.ogmiosConfig.port, "ogmios"), new Data_Tuple.Tuple(cfg.ogmiosDatumCacheConfig.port, "ogmios-datum-cache"), new Data_Tuple.Tuple(cfg.postgresConfig.port, "postgres") ])(Data_Foldable.foldMap(Data_Foldable.foldableMaybe)(Data_Monoid.monoidArray)((function () {
        var $88 = Control_Applicative.pure(Control_Applicative.applicativeArray);
        return function ($89) {
            return $88((function (v) {
                return new Data_Tuple.Tuple(v, "ctl-server");
            })((function (v) {
                return v.port;
            })($89)));
        };
    })())(cfg.ctlServerConfig));
    return Control_Bind.bind(Effect_Aff.bindAff)(Data_Functor.map(Effect_Aff.functorAff)(Data_Array.catMaybes)(Data_Traversable["for"](Effect_Aff.applicativeAff)(Data_Traversable.traversableArray)(services)(function (v) {
        return Data_Functor.mapFlipped(Effect_Aff.functorAff)(Plutip_PortCheck.isPortAvailable(v.value0))(function (v1) {
            if (v1) {
                return Data_Maybe.Nothing.value;
            };
            return new Data_Maybe.Just(new Data_Tuple.Tuple(v.value0, v.value1));
        });
    })))(function (occupiedServices) {
        return Control_Applicative.unless(Effect_Aff.applicativeAff)(Data_Array["null"](occupiedServices))(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception["throw"]("Unable to run the following services, because the ports are occupied:\x0a" + Data_Foldable.foldMap(Data_Foldable.foldableArray)(Data_Monoid.monoidString)(printServiceEntry)(occupiedServices))));
    });
};
var withPlutipContractEnv = function (dictUtxoDistribution) {
    return function (plutipCfg) {
        return function (distr) {
            return function (cont) {
                var withWallets = function (env) {
                    return function (ourKey) {
                        return function (response) {
                            return function (cc) {
                                return Control_Bind.bind(Effect_Aff.bindAff)(Contract_Monad.runContractInEnv(Data_Newtype.over()()(Data_Newtype.wrap())(function (v) {
                                    var $47 = {};
                                    for (var $48 in v) {
                                        if ({}.hasOwnProperty.call(v, $48)) {
                                            $47[$48] = v[$48];
                                        };
                                    };
                                    $47.config = (function () {
                                        var $44 = {};
                                        for (var $45 in v.config) {
                                            if ({}.hasOwnProperty.call(v.config, $45)) {
                                                $44[$45] = v["config"][$45];
                                            };
                                        };
                                        $44.customLogger = new Data_Maybe.Just(Data_Function["const"](Control_Applicative.pure(Effect_Aff.applicativeAff)(Data_Unit.unit)));
                                        return $44;
                                    })();
                                    return $47;
                                })(env))(Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("Impossible happened: could not decode wallets. Please report as bug")(Plutip_UtxoDistribution.decodeWallets(dictUtxoDistribution)(distr)(response.privateKeys)))(function (wallets) {
                                    var walletsArray = Plutip_UtxoDistribution.keyWallets(dictUtxoDistribution)(Type_Proxy["Proxy"].value)(wallets);
                                    return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Plutip_UtxoDistribution.transferFundsFromEnterpriseToBase(ourKey)(walletsArray))(function () {
                                        return Control_Applicative.pure(Contract_Monad.applicativeContract)(wallets);
                                    });
                                })))(function (wallets) {
                                    return cc(wallets);
                                });
                            };
                        };
                    };
                };
                var withPostgres = function (response) {
                    var $90 = Effect_Aff.bracket(startPostgresServer(plutipCfg.postgresConfig)(response))(stopChildProcessWithPort(plutipCfg.postgresConfig.port));
                    return function ($91) {
                        return $90(Data_Function["const"]($91));
                    };
                };
                var withPlutipServer = (function () {
                    var $92 = Effect_Aff.bracket(startPlutipServer(plutipCfg))(stopChildProcessWithPort(plutipCfg.port));
                    return function ($93) {
                        return $92(Data_Function["const"]($93));
                    };
                })();
                var withPlutipCluster = function (cc) {
                    var distrArray = Plutip_UtxoDistribution.encodeDistribution(Plutip_UtxoDistribution["utxoDistribution/\\/\\"](Plutip_UtxoDistribution.utxoDistributionInitialUT)(dictUtxoDistribution))(new Data_Tuple.Tuple(ourInitialUtxos(Plutip_UtxoDistribution.encodeDistribution(dictUtxoDistribution)(distr)), distr));
                    return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Data_Foldable.for_(Effect_Aff.applicativeAff)(Data_Foldable.foldableArray)(distrArray)(Data_Foldable.traverse_(Effect_Aff.applicativeAff)(Data_Foldable.foldableArray)(function (n) {
                        return Control_Applicative.when(Effect_Aff.applicativeAff)(Data_Ord.lessThan(Data_BigInt.ordBigInt)(n)(Data_BigInt.fromInt(1000000)))(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(Effect_Exception["throw"]("UTxO is too low: " + (Data_BigInt.toString(n) + ", must be at least 1_000_000 Lovelace"))));
                    })))(function () {
                        return Effect_Aff.bracket(startPlutipCluster(plutipCfg)(distrArray))(Data_Function["const"](Data_Functor["void"](Effect_Aff.functorAff)(stopPlutipCluster(plutipCfg))))(cc);
                    });
                };
                var withOgmiosDatumCache = function (response) {
                    var $94 = Effect_Aff.bracket(startOgmiosDatumCache(plutipCfg)(response))(stopChildProcessWithPort(plutipCfg.ogmiosDatumCacheConfig.port));
                    return function ($95) {
                        return $94(Data_Function["const"]($95));
                    };
                };
                var withOgmios = function (response) {
                    var $96 = Effect_Aff.bracket(startOgmios(plutipCfg)(response))(stopChildProcessWithPort(plutipCfg.ogmiosConfig.port));
                    return function ($97) {
                        return $96(Data_Function["const"]($97));
                    };
                };
                var withMCtlServer = function (x) {
                    if (plutipCfg.ctlServerConfig instanceof Data_Maybe.Nothing) {
                        return x;
                    };
                    if (plutipCfg.ctlServerConfig instanceof Data_Maybe.Just) {
                        return Effect_Aff.bracket(startCtlServer(plutipCfg.ctlServerConfig.value0.port))(stopChildProcessWithPort(plutipCfg.ctlServerConfig.value0.port))(Data_Function["const"](x));
                    };
                    throw new Error("Failed pattern match at Plutip.Server (line 173, column 22 - line 179, column 18): " + [ plutipCfg.ctlServerConfig.constructor.name ]);
                };
                var stopContractEnv = function (env) {
                    return QueryM.stopQueryRuntime((Data_Newtype.unwrap()(env)).runtime);
                };
                var withContractEnv = (function () {
                    if (plutipCfg.suppressLogs) {
                        return function (contractEnvCont) {
                            return Control_Bind.bind(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(QueryM_Logging.setupLogs(plutipCfg.logLevel)(plutipCfg.customLogger)))(function (v) {
                                var configLogger = Data_Maybe.Just.create((function () {
                                    var $98 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
                                    return function ($99) {
                                        return $98(v.addLogEntry($99));
                                    };
                                })());
                                return Effect_Aff.bracket(mkClusterContractEnv(plutipCfg)(v.suppressedLogger)(configLogger))((function () {
                                    var $100 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
                                    return function ($101) {
                                        return $100(stopContractEnv($101));
                                    };
                                })())(Control_Bind.composeKleisli(Effect_Aff.bindAff)((function () {
                                    var $102 = Control_Monad_Error_Class["try"](Effect_Aff.monadErrorAff);
                                    return function ($103) {
                                        return $102(contractEnvCont($103));
                                    };
                                })())(function (v1) {
                                    if (v1 instanceof Data_Either.Left) {
                                        return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(Effect_Class.liftEffect(Effect_Aff.monadEffectAff)(v.printLogs))(function () {
                                            return Control_Monad_Error_Class.throwError(Effect_Aff.monadThrowAff)(v1.value0);
                                        });
                                    };
                                    if (v1 instanceof Data_Either.Right) {
                                        return Control_Applicative.pure(Effect_Aff.applicativeAff)(v1.value0);
                                    };
                                    throw new Error("Failed pattern match at Plutip.Server (line 211, column 37 - line 215, column 32): " + [ v1.constructor.name ]);
                                }));
                            });
                        };
                    };
                    return Effect_Aff.bracket(mkClusterContractEnv(plutipCfg)(QueryM.mkLogger(plutipCfg.logLevel)(plutipCfg.customLogger))(plutipCfg.customLogger))((function () {
                        var $104 = Effect_Class.liftEffect(Effect_Aff.monadEffectAff);
                        return function ($105) {
                            return $104(stopContractEnv($105));
                        };
                    })());
                })();
                return Control_Bind.discard(Control_Bind.discardUnit)(Effect_Aff.bindAff)(configCheck(plutipCfg))(function () {
                    return withPlutipServer(withPlutipCluster(function (v) {
                        return withPostgres(v.value1)(withOgmios(v.value1)(withOgmiosDatumCache(v.value1)(withMCtlServer(withContractEnv(function (env) {
                            return withWallets(env)(v.value0)(v.value1)(cont(env));
                        })))));
                    }));
                });
            };
        };
    };
};
var runPlutipContract = function (dictUtxoDistribution) {
    return function (cfg) {
        return function (distr) {
            return function (cont) {
                return withPlutipContractEnv(dictUtxoDistribution)(cfg)(distr)(function (env) {
                    return function (wallets) {
                        return Contract_Monad.runContractInEnv(env)(cont(wallets));
                    };
                });
            };
        };
    };
};
module.exports = {
    runPlutipContract: runPlutipContract,
    withPlutipContractEnv: withPlutipContractEnv,
    startPlutipCluster: startPlutipCluster,
    stopPlutipCluster: stopPlutipCluster,
    startPlutipServer: startPlutipServer,
    stopChildProcessWithPort: stopChildProcessWithPort
};