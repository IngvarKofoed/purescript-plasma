// Generated by purs version 0.11.4
"use strict";
var Data_EuclideanRing = require("../Data.EuclideanRing");
var Data_HeytingAlgebra = require("../Data.HeytingAlgebra");
var Data_Ord = require("../Data.Ord");
var Data_Ring = require("../Data.Ring");
var Data_Semiring = require("../Data.Semiring");
var $$Math = require("../Math");
var Prelude = require("../Prelude");
var Tolerance = function (x) {
    return x;
};
var Fraction = function (x) {
    return x;
};
var eqRelative = function (v) {
    return function (v1) {
        return function (v2) {
            if (v1 === 0.0) {
                return $$Math.abs(v2) <= v;
            };
            if (v2 === 0.0) {
                return $$Math.abs(v1) <= v;
            };
            return $$Math.abs(v1 - v2) <= (v * $$Math.abs(v1 + v2)) / 2.0;
        };
    };
};
var eqApproximate = eqRelative(1.0e-6);
var neqApproximate = function (x) {
    return function (y) {
        return !eqApproximate(x)(y);
    };
};
var eqAbsolute = function (v) {
    return function (x) {
        return function (y) {
            return $$Math.abs(x - y) <= v;
        };
    };
};
module.exports = {
    Fraction: Fraction, 
    Tolerance: Tolerance, 
    eqAbsolute: eqAbsolute, 
    eqApproximate: eqApproximate, 
    eqRelative: eqRelative, 
    neqApproximate: neqApproximate
};
