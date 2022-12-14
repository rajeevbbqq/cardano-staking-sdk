// Generated by purs version 0.14.5
"use strict";
var Grating = function (x) {
    return x;
};
var profunctorGrating = {
    dimap: function (f) {
        return function (g) {
            return function (v) {
                return function (d) {
                    return g(v(function (k) {
                        return d(function ($6) {
                            return k(f($6));
                        });
                    }));
                };
            };
        };
    }
};
var newtypeGrating = {
    Coercible0: function () {
        return undefined;
    }
};
var closedGrating = {
    closed: function (v) {
        return function (f) {
            return function (x) {
                return v(function (k) {
                    return f(function (g) {
                        return k(g(x));
                    });
                });
            };
        };
    },
    Profunctor0: function () {
        return profunctorGrating;
    }
};
module.exports = {
    Grating: Grating,
    newtypeGrating: newtypeGrating,
    profunctorGrating: profunctorGrating,
    closedGrating: closedGrating
};
