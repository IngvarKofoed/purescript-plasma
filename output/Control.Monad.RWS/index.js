// Generated by purs version 0.11.4
"use strict";
var Control_Applicative = require("../Control.Applicative");
var Control_Monad_RWS_Trans = require("../Control.Monad.RWS.Trans");
var Control_Monad_Reader_Class = require("../Control.Monad.Reader.Class");
var Control_Monad_State_Class = require("../Control.Monad.State.Class");
var Control_Monad_Trans_Class = require("../Control.Monad.Trans.Class");
var Control_Monad_Writer_Class = require("../Control.Monad.Writer.Class");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var Data_Function = require("../Data.Function");
var Data_Identity = require("../Data.Identity");
var Data_Newtype = require("../Data.Newtype");
var Data_Tuple = require("../Data.Tuple");
var Prelude = require("../Prelude");
var withRWS = Control_Monad_RWS_Trans.withRWST;
var rws = function (f) {
    return function (r) {
        return function (s) {
            return Control_Applicative.pure(Data_Identity.applicativeIdentity)(f(r)(s));
        };
    };
};
var runRWS = function (m) {
    return function (r) {
        return function (s) {
            var v = m(r)(s);
            return v;
        };
    };
};
var mapRWS = function (f) {
    return Control_Monad_RWS_Trans.mapRWST(function ($3) {
        return Data_Identity.Identity(f(Data_Newtype.unwrap(Data_Identity.newtypeIdentity)($3)));
    });
};
var execRWS = function (m) {
    return function (r) {
        return function (s) {
            return Data_Newtype.unwrap(Data_Identity.newtypeIdentity)(Control_Monad_RWS_Trans.execRWST(Data_Identity.monadIdentity)(m)(r)(s));
        };
    };
};
var evalRWS = function (m) {
    return function (r) {
        return function (s) {
            return Data_Newtype.unwrap(Data_Identity.newtypeIdentity)(Control_Monad_RWS_Trans.evalRWST(Data_Identity.monadIdentity)(m)(r)(s));
        };
    };
};
module.exports = {
    evalRWS: evalRWS, 
    execRWS: execRWS, 
    mapRWS: mapRWS, 
    runRWS: runRWS, 
    rws: rws, 
    withRWS: withRWS
};
