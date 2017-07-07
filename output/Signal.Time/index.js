// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_Eff_Timer = require("../Control.Monad.Eff.Timer");
var Data_Eq = require("../Data.Eq");
var Data_Function = require("../Data.Function");
var Prelude = require("../Prelude");
var Signal = require("../Signal");
var since = $foreign.sinceP(Signal.constant);
var second = 1000.0;
var millisecond = 1.0;
var every = $foreign.everyP(Signal.constant);
var delay = $foreign.delayP(Signal.constant);
var debounce = function (t) {
    return function (s) {
        var whenEqual = function (value) {
            return function (input) {
                return Signal.filter(Data_Eq.eq(Data_Eq.eqBoolean)(value))(value)(input);
            };
        };
        var whenChangeTo = function (value) {
            return function (input) {
                return whenEqual(value)(Signal.dropRepeats(Data_Eq.eqBoolean)(input));
            };
        };
        var leading = whenChangeTo(false)(since(t)(s));
        return Signal.sampleOn(leading)(s);
    };
};
module.exports = {
    debounce: debounce, 
    delay: delay, 
    every: every, 
    millisecond: millisecond, 
    second: second, 
    since: since, 
    now: $foreign.now
};
