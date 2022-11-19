// Generated by purs version 0.14.5
"use strict";
var Contract_Monad = require("../Contract.Monad/index.js");
var Control_Applicative = require("../Control.Applicative/index.js");
var Control_Bind = require("../Control.Bind/index.js");
var Data_Array = require("../Data.Array/index.js");
var Data_BigInt = require("../Data.BigInt/index.js");
var Data_Eq = require("../Data.Eq/index.js");
var Data_Foldable = require("../Data.Foldable/index.js");
var Data_Functor = require("../Data.Functor/index.js");
var Data_Maybe = require("../Data.Maybe/index.js");
var Data_Ord = require("../Data.Ord/index.js");
var Data_Ring = require("../Data.Ring/index.js");
var Data_Semiring = require("../Data.Semiring/index.js");
var Data_Show = require("../Data.Show/index.js");
var Data_Tuple = require("../Data.Tuple/index.js");
var Types_Interval = require("../Types.Interval/index.js");
var Types_Rational = require("../Types.Rational/index.js");
var Utils = require("../Utils/index.js");
var mkUnbondedPoolParams = function (admin) {
    return function (nftCs) {
        return function (assocListCs) {
            return function (v) {
                return {
                    start: v.start,
                    userLength: v.userLength,
                    adminLength: v.adminLength,
                    bondingLength: v.bondingLength,
                    interestLength: v.interestLength,
                    increments: v.increments,
                    interest: v.interest,
                    minStake: v.minStake,
                    maxStake: v.maxStake,
                    admin: admin,
                    unbondedAssetClass: v.unbondedAssetClass,
                    nftCs: nftCs,
                    assocListCs: assocListCs
                };
            };
        };
    };
};
var isWithinPeriod = function (currTime) {
    return function (cycleLength) {
        return function (start) {
            return function (end) {
                var currentIteration = Data_Maybe.fromMaybe(0)(Data_BigInt.toInt(Data_BigInt.quot(start)(cycleLength)));
                var upperBound = 1 + currentIteration | 0;
                var possibleRanges = Data_Functor.map(Data_Functor.functorArray)(function (i) {
                    return new Data_Tuple.Tuple(Data_Semiring.add(Data_BigInt.semiringBigInt)(Data_Semiring.mul(Data_BigInt.semiringBigInt)(cycleLength)(Utils.big(i - 1 | 0)))(start), Data_Semiring.add(Data_BigInt.semiringBigInt)(Data_Semiring.mul(Data_BigInt.semiringBigInt)(cycleLength)(Utils.big(i - 1 | 0)))(end));
                })(Data_Array.range(1)(upperBound));
                var periods = Data_Array.takeWhile(function (v) {
                    return Data_Ord.greaterThanOrEq(Data_BigInt.ordBigInt)(currTime)(v.value0);
                })(possibleRanges);
                var currentPeriod = Data_Array.filter(function (v) {
                    return Data_Ord.lessThanOrEq(Data_BigInt.ordBigInt)(v.value0)(currTime) && Data_Ord.lessThan(Data_BigInt.ordBigInt)(currTime)(v.value1);
                })(periods);
                var $23 = Data_Foldable["null"](Data_Foldable.foldableArray)(currentPeriod);
                if ($23) {
                    return Data_Maybe.Nothing.value;
                };
                return Data_Array.head(currentPeriod);
            };
        };
    };
};
var getUserTime = function (v) {
    return Control_Bind.bind(Contract_Monad.bindContract)(Utils.currentRoundedTime)(function (v1) {
        var userEnd = Data_Ring.sub(Data_BigInt.ringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(v.start)(v.userLength))(Utils.big(1000));
        var cycleLength = Data_Semiring.add(Data_BigInt.semiringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(v.userLength)(v.adminLength))(v.bondingLength);
        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("getUserTime: this is not a user period")(isWithinPeriod(v1)(cycleLength)(v.start)(userEnd)))(function (v2) {
            return Control_Applicative.pure(Contract_Monad.applicativeContract)({
                currTime: v1,
                range: Types_Interval.interval(v2.value0)(v2.value1)
            });
        });
    });
};
var getBondingTime = function (v) {
    return Control_Bind.bind(Contract_Monad.bindContract)(Utils.currentRoundedTime)(function (v1) {
        var userEnd = Data_Ring.sub(Data_BigInt.ringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(v.start)(v.userLength))(Utils.big(1000));
        var getPeriod = function (user) {
            return function (bonding) {
                var v2 = new Data_Tuple.Tuple(user, bonding);
                if (v2.value1 instanceof Data_Maybe.Nothing) {
                    return v2.value0;
                };
                if (v2.value0 instanceof Data_Maybe.Nothing) {
                    return v2.value1;
                };
                return Data_Maybe.Nothing.value;
            };
        };
        var cycleLength = Data_Semiring.add(Data_BigInt.semiringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(v.userLength)(v.adminLength))(v.bondingLength);
        var userPeriod = isWithinPeriod(v1)(cycleLength)(v.start)(userEnd);
        var bondingStart = Data_Semiring.add(Data_BigInt.semiringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(v.start)(v.userLength))(v.adminLength);
        var bondingEnd = Data_Ring.sub(Data_BigInt.ringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(bondingStart)(v.bondingLength))(Utils.big(1000));
        var bondingPeriod = isWithinPeriod(v1)(cycleLength)(bondingStart)(bondingEnd);
        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("getUserTime: this is not a user/bonding period")(getPeriod(userPeriod)(bondingPeriod)))(function (v2) {
            return Control_Applicative.pure(Contract_Monad.applicativeContract)({
                currTime: v1,
                range: Types_Interval.interval(v2.value0)(v2.value1)
            });
        });
    });
};
var getAdminTime = function (v) {
    return Control_Bind.bind(Contract_Monad.bindContract)(Utils.currentRoundedTime)(function (v1) {
        var cycleLength = Data_Semiring.add(Data_BigInt.semiringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(v.userLength)(v.adminLength))(v.bondingLength);
        var adminStart = Data_Semiring.add(Data_BigInt.semiringBigInt)(v.start)(v.userLength);
        var adminEnd = Data_Ring.sub(Data_BigInt.ringBigInt)(Data_Semiring.add(Data_BigInt.semiringBigInt)(adminStart)(v.adminLength))(Utils.big(1000));
        return Control_Bind.bind(Contract_Monad.bindContract)(Contract_Monad.liftContractM("getAdminTime: this is not a admin period")(isWithinPeriod(v1)(cycleLength)(adminStart)(adminEnd)))(function (v2) {
            return Control_Applicative.pure(Contract_Monad.applicativeContract)({
                currTime: v1,
                range: Types_Interval.interval(v2.value0)(v2.value1)
            });
        });
    });
};
var calculateRewards = function (rewards) {
    return function (totalRewards) {
        return function (deposited) {
            return function (newDeposit) {
                return function (totalDeposited) {
                    var $46 = Data_Eq.eq(Data_BigInt.eqBigInt)(totalDeposited)(Data_Semiring.zero(Data_BigInt.semiringBigInt));
                    if ($46) {
                        return Control_Applicative.pure(Contract_Monad.applicativeContract)(Data_Semiring.zero(Types_Rational.semiringRational));
                    };
                    var rhs = Data_Semiring.add(Types_Rational.semiringRational)(rewards)(Utils.mkRatUnsafe(Types_Rational.reduce(Types_Rational.rationalComponentBigInt)(deposited)(Data_Semiring.one(Data_BigInt.semiringBigInt))));
                    var rhs$prime = Data_Ring.sub(Types_Rational.ringRational)(rhs)(Utils.mkRatUnsafe(Types_Rational.reduce(Types_Rational.rationalComponentBigInt)(newDeposit)(Data_Semiring.one(Data_BigInt.semiringBigInt))));
                    var lhs = Utils.mkRatUnsafe(Types_Rational.reduce(Types_Rational.rationalComponentBigInt)(totalRewards)(totalDeposited));
                    var f = Data_Semiring.mul(Types_Rational.semiringRational)(rhs$prime)(lhs);
                    return Control_Bind.discard(Control_Bind.discardUnit)(Contract_Monad.bindContract)(Control_Applicative.when(Contract_Monad.applicativeContract)(Data_Ord.lessThan(Types_Rational.ordRational)(f)(Data_Semiring.zero(Types_Rational.semiringRational)))(Contract_Monad.throwContractError(Data_Show.showString)("calculateRewards: invalid rewards amount")))(function () {
                        return Control_Applicative.pure(Contract_Monad.applicativeContract)(Data_Semiring.add(Types_Rational.semiringRational)(rewards)(f));
                    });
                };
            };
        };
    };
};
module.exports = {
    calculateRewards: calculateRewards,
    getAdminTime: getAdminTime,
    getBondingTime: getBondingTime,
    getUserTime: getUserTime,
    mkUnbondedPoolParams: mkUnbondedPoolParams
};
