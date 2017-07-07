// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Applicative = require("../Control.Applicative");
var Control_Bind = require("../Control.Bind");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Monad_ST = require("../Control.Monad.ST");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Maybe = require("../Data.Maybe");
var Prelude = require("../Prelude");
var Unsafe_Coerce = require("../Unsafe.Coerce");
var unsafeFreeze = function ($7) {
    return Control_Applicative.pure(Control_Monad_Eff.applicativeEff)(Unsafe_Coerce.unsafeCoerce($7));
};
var thaw = $foreign.copyImpl;
var withArray = function (f) {
    return function (xs) {
        return function __do() {
            var v = thaw(xs)();
            var v1 = f(v)();
            return unsafeFreeze(v)();
        };
    };
};
var pushSTArray = function (arr) {
    return function (a) {
        return $foreign.pushAllSTArray(arr)([ a ]);
    };
};
var peekSTArray = $foreign.peekSTArrayImpl(Data_Maybe.Just.create)(Data_Maybe.Nothing.value);
var modifySTArray = function (xs) {
    return function (i) {
        return function (f) {
            return function __do() {
                var v = peekSTArray(xs)(i)();
                if (v instanceof Data_Maybe.Just) {
                    return $foreign.pokeSTArray(xs)(i)(f(v.value0))();
                };
                if (v instanceof Data_Maybe.Nothing) {
                    return false;
                };
                throw new Error("Failed pattern match at Data.Array.ST line 120, column 3 - line 122, column 26: " + [ v.constructor.name ]);
            };
        };
    };
};
var freeze = $foreign.copyImpl;
module.exports = {
    freeze: freeze, 
    modifySTArray: modifySTArray, 
    peekSTArray: peekSTArray, 
    pushSTArray: pushSTArray, 
    thaw: thaw, 
    unsafeFreeze: unsafeFreeze, 
    withArray: withArray, 
    emptySTArray: $foreign.emptySTArray, 
    pokeSTArray: $foreign.pokeSTArray, 
    pushAllSTArray: $foreign.pushAllSTArray, 
    runSTArray: $foreign.runSTArray, 
    spliceSTArray: $foreign.spliceSTArray, 
    toAssocArray: $foreign.toAssocArray
};
