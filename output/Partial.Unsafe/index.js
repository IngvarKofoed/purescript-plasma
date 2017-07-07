// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Partial = require("../Partial");
var unsafePartialBecause = function (v) {
    return function (x) {
        return $foreign.unsafePartial(function (dictPartial) {
            return x(dictPartial);
        });
    };
};
var unsafeCrashWith = function (msg) {
    return $foreign.unsafePartial(function (dictPartial) {
        return Partial.crashWith(dictPartial)(msg);
    });
};
module.exports = {
    unsafeCrashWith: unsafeCrashWith, 
    unsafePartialBecause: unsafePartialBecause, 
    unsafePartial: $foreign.unsafePartial
};