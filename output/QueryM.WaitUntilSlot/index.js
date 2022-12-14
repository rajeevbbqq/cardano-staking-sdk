// Generated by purs version 0.14.5
"use strict";
var Contract_Log = require("../Contract.Log/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Control_Monad_Error_Class = require("../Control.Monad.Error.Class/index.js");
var Data_Bifunctor = require("../Data.Bifunctor/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Boolean = require("../Data.Boolean/index.js");
var Data_DateTime_Instant = require("../Data.DateTime.Instant/index.js");
var Data_Either = require("../Data.Either/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_EuclideanRing = require("../Data.EuclideanRing/index.js");
var Data_Function = require("../Data.Function/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Int = require("../Data.Int/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Newtype = require("../Data.Newtype/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Effect = require("../Effect/index.js");
var Effect_Aff = require("../Effect.Aff/index.js");
var Effect_Aff_Class = require("../Effect.Aff.Class/index.js");
var Effect_Class = require("../Effect.Class/index.js");
var Effect_Exception = require("../Effect.Exception/index.js");
var Effect_Now = require("../Effect.Now/index.js");
var Helpers = require("../Helpers/index.js");
var QueryM = require("../QueryM/index.js");
var QueryM_EraSummaries = require("../QueryM.EraSummaries/index.js");
var QueryM_SystemStart = require("../QueryM.SystemStart/index.js");
var Serialization_Address = require("../Serialization.Address/index.js");
var Types_BigNum = require("../Types.BigNum/index.js");
var Types_Chain = require("../Types.Chain/index.js");
var Types_Interval = require("../Types.Interval/index.js");
var Types_Natural = require("../Types.Natural/index.js");
var slotToEndPOSIXTime = function (slot) {
    return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to advance slot"))(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap())(Types_BigNum.add(Data_Newtype.unwrap()(slot))(Types_BigNum.fromInt(1)))))(function (futureSlot) {
        return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(QueryM_EraSummaries.getEraSummaries)(function (eraSummaries) {
            return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(QueryM_SystemStart.getSystemStart)(function (sysStart) {
                return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Class.liftEffect(QueryM.monadEffectQueryMExtended)(Control_Bind.bind(Effect.bindEffect)(Types_Interval.slotToPosixTime(eraSummaries)(sysStart)(futureSlot))((function () {
                    var $19 = Helpers.liftM(Control_Monad_Error_Class.monadErrorEffect)(Effect_Exception.error("Unable to convert Slot to POSIXTime"));
                    return function ($20) {
                        return $19(Data_Either.hush($20));
                    };
                })())))(function (futureTime) {
                    return Control_Applicative.pure(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))(Data_Semiring.add(Types_Interval.semiringPOSIXTime)(Data_Newtype.wrap()(Data_BigInt.fromInt(-1 | 0)))(futureTime));
                });
            });
        });
    });
};
var posixTimeToSeconds = function (v) {
    return Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to convert POSIXTIme to Number"))(Data_Functor.map(Data_Maybe.functorMaybe)((function () {
        var $21 = Data_Newtype.wrap();
        return function ($22) {
            return $21(Data_Int.toNumber($22));
        };
    })())(Data_BigInt.toInt(Data_EuclideanRing.div(Data_BigInt.euclideanRingBigInt)(v)(Data_BigInt.fromInt(1000)))));
};
var getLag = function (eraSummaries) {
    return function (sysStart) {
        return function (nowSlot) {
            return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Class.liftEffect(QueryM.monadEffectQueryMExtended)(Types_Interval.slotToPosixTime(eraSummaries)(sysStart)(nowSlot)))((function () {
                var $23 = Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to convert Slot to POSIXTime"));
                return function ($24) {
                    return $23(Data_Either.hush($24));
                };
            })()))(function (nowPosixTime) {
                return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Data_Functor.map(QueryM.functorQueryMExtended(Effect_Aff.functorAff))((function () {
                    var $25 = Data_Newtype.unwrap();
                    return function ($26) {
                        return $25(Data_DateTime_Instant.unInstant($26));
                    };
                })())(Effect_Class.liftEffect(QueryM.monadEffectQueryMExtended)(Effect_Now.now)))(function (nowMs) {
                    return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Contract_Log["logTrace'"](QueryM.monadLoggerQueryMExtended)("getLag: current slot: " + (Types_BigNum.toString(Data_Newtype.unwrap()(nowSlot)) + (", slot time: " + (Data_BigInt.toString(Data_Newtype.unwrap()(nowPosixTime)) + (", system time: " + Data_Show.show(Data_Show.showNumber)(nowMs)))))))(function () {
                        return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to convert Milliseconds to BigInt"))(Data_BigInt.fromNumber(nowMs)))(function (nowMsBigInt) {
                            return Control_Applicative.pure(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))(Data_Newtype.wrap()(Data_BigInt.toNumber(Data_Ring.sub(Data_BigInt.ringBigInt)(nowMsBigInt)(Data_Newtype.unwrap()(nowPosixTime)))));
                        });
                    });
                });
            });
        };
    };
};
var estimateDelayUntil = function (futureTimePosix) {
    var nonNegative = function (n) {
        if (n < 0.0) {
            return 0.0;
        };
        if (Data_Boolean.otherwise) {
            return n;
        };
        throw new Error("Failed pattern match at QueryM.WaitUntilSlot (line 128, column 3 - line 128, column 34): " + [ n.constructor.name ]);
    };
    return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(posixTimeToSeconds(futureTimePosix))(function (futureTimeSec) {
        return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Data_Functor.map(QueryM.functorQueryMExtended(Effect_Aff.functorAff))((function () {
            var $27 = Data_Newtype.unwrap();
            return function ($28) {
                return $27(Data_DateTime_Instant.unInstant($28));
            };
        })())(Effect_Class.liftEffect(QueryM.monadEffectQueryMExtended)(Effect_Now.now)))(function (nowMs) {
            var result = Data_Newtype.wrap()(1000.0 * nonNegative(Data_Newtype.unwrap()(futureTimeSec) - nowMs / 1000.0));
            return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Contract_Log["logTrace'"](QueryM.monadLoggerQueryMExtended)("estimateDelayUntil: target time: " + (Data_Show.show(Data_Show.showNumber)(Data_Newtype.unwrap()(futureTimeSec) * 1000.0) + (", system time: " + (Data_Show.show(Data_Show.showNumber)(nowMs) + (", delay: " + (Data_Show.show(Data_Show.showNumber)(Data_Newtype.unwrap()(result)) + "ms")))))))(function () {
                return Control_Applicative.pure(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))(result);
            });
        });
    });
};
var waitUntilSlot = function (futureSlot) {
    var retryDelay = Data_Newtype.wrap()(1000.0);
    var logLag = function (slotLengthMs) {
        return function (v) {
            return Contract_Log["logTrace'"](QueryM.monadLoggerQueryMExtended)("waitUntilSlot: current lag: " + (Data_Show.show(Data_Show.showNumber)(v) + (" ms, " + (Data_Show.show(Data_Show.showNumber)(v / slotLengthMs) + " slots."))));
        };
    };
    return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(QueryM.getChainTip)(function (v) {
        if (v instanceof Types_Chain.Tip) {
            if (Data_Ord.greaterThanOrEq(Serialization_Address.ordSlot)(v.value0.slot)(futureSlot)) {
                return Control_Applicative.pure(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))(v);
            };
            if (Data_Boolean.otherwise) {
                return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(QueryM_EraSummaries.getEraSummaries)(function (eraSummaries) {
                    return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(QueryM_SystemStart.getSystemStart)(function (sysStart) {
                        return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Data_Functor.map(QueryM.functorQueryMExtended(Effect_Aff.functorAff))(Types_Interval.getSlotLength)(Helpers.liftEither(QueryM.monadErrorErrorQueryMExte)(Data_Bifunctor.lmap(Data_Bifunctor.bifunctorEither)(Data_Function["const"](Effect_Exception.error("Unable to get current Era summary")))(Types_Interval.findSlotEraSummary(eraSummaries)(v.value0.slot)))))(function (slotLengthMs) {
                            return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(getLag(eraSummaries)(sysStart)(v.value0.slot))(logLag(slotLengthMs)))(function () {
                                return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Class.liftEffect(QueryM.monadEffectQueryMExtended)(Types_Interval.slotToPosixTime(eraSummaries)(sysStart)(futureSlot)))((function () {
                                    var $29 = Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to convert Slot to POSIXTime"));
                                    return function ($30) {
                                        return $29(Data_Either.hush($30));
                                    };
                                })()))(function (futureTime) {
                                    return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(estimateDelayUntil(futureTime))(function (delayTime) {
                                        return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Aff_Class.liftAff(QueryM.monadAffQueryMExtendedAff)(Effect_Aff.delay(delayTime)))(function () {
                                            var fetchRepeatedly = Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(QueryM.getChainTip)(function (v1) {
                                                if (v1 instanceof Types_Chain.Tip) {
                                                    if (Data_Ord.greaterThanOrEq(Serialization_Address.ordSlot)(v1.value0.slot)(futureSlot)) {
                                                        return Control_Applicative.pure(QueryM.applicativeQueryMExtended(Effect_Aff.applicativeAff))(v1);
                                                    };
                                                    if (Data_Boolean.otherwise) {
                                                        return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Aff_Class.liftAff(QueryM.monadAffQueryMExtendedAff)(Effect_Aff.delay(slotLengthMs)))(function () {
                                                            return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(getLag(eraSummaries)(sysStart)(v1.value0.slot))(logLag(slotLengthMs)))(function () {
                                                                return fetchRepeatedly;
                                                            });
                                                        });
                                                    };
                                                };
                                                if (v1 instanceof Types_Chain.TipAtGenesis) {
                                                    return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Aff_Class.liftAff(QueryM.monadAffQueryMExtendedAff)(Effect_Aff.delay(retryDelay)))(function () {
                                                        return fetchRepeatedly;
                                                    });
                                                };
                                                throw new Error("Failed pattern match at QueryM.WaitUntilSlot (line 66, column 31 - line 76, column 34): " + [ v1.constructor.name ]);
                                            });
                                            return fetchRepeatedly;
                                        });
                                    });
                                });
                            });
                        });
                    });
                });
            };
        };
        if (v instanceof Types_Chain.TipAtGenesis) {
            return Control_Bind.discard(Control_Bind.discardUnit)(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Effect_Aff_Class.liftAff(QueryM.monadAffQueryMExtendedAff)(Effect_Aff.delay(retryDelay)))(function () {
                return waitUntilSlot(futureSlot);
            });
        };
        throw new Error("Failed pattern match at QueryM.WaitUntilSlot (line 43, column 19 - line 81, column 31): " + [ v.constructor.name ]);
    });
};
var currentSlot = Data_Functor.mapFlipped(QueryM.functorQueryMExtended(Effect_Aff.functorAff))(QueryM.getChainTip)(function (v) {
    if (v instanceof Types_Chain.Tip) {
        return v.value0.slot;
    };
    if (v instanceof Types_Chain.TipAtGenesis) {
        return Serialization_Address.Slot(Types_BigNum.fromInt(0));
    };
    throw new Error("Failed pattern match at QueryM.WaitUntilSlot (line 153, column 31 - line 155, column 52): " + [ v.constructor.name ]);
});
var currentTime = Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(currentSlot)(slotToEndPOSIXTime);
var waitNSlots = function (offset) {
    return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to convert BigInt to BigNum"))(Types_BigNum.fromBigInt(Types_Natural.toBigInt(offset))))(function (offsetBigNum) {
        var $18 = Data_Eq.eq(Types_BigNum.eqBigNum)(offsetBigNum)(Types_BigNum.fromInt(0));
        if ($18) {
            return QueryM.getChainTip;
        };
        return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(currentSlot)(function (slot) {
            return Control_Bind.bind(QueryM.bindQueryMExtended(Effect_Aff.bindAff))(Helpers.liftM(QueryM.monadErrorErrorQueryMExte)(Effect_Exception.error("Unable to advance slot"))(Data_Functor.map(Data_Maybe.functorMaybe)(Data_Newtype.wrap())(Types_BigNum.add(Data_Newtype.unwrap()(slot))(offsetBigNum))))(function (newSlot) {
                return waitUntilSlot(newSlot);
            });
        });
    });
};
module.exports = {
    waitUntilSlot: waitUntilSlot,
    waitNSlots: waitNSlots,
    currentSlot: currentSlot,
    currentTime: currentTime
};
