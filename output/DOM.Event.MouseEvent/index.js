// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_Event_Types = require("../DOM.Event.Types");
var Data_Foreign = require("../Data.Foreign");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var relatedTarget = Data_Functor.map(Data_Functor.functorFn)(Data_Nullable.toMaybe)($foreign._relatedTarget);
var eventToMouseEvent = function ($0) {
    return DOM_Event_Types.readMouseEvent(Data_Foreign.toForeign($0));
};
module.exports = {
    eventToMouseEvent: eventToMouseEvent, 
    relatedTarget: relatedTarget, 
    altKey: $foreign.altKey, 
    button: $foreign.button, 
    buttons: $foreign.buttons, 
    clientX: $foreign.clientX, 
    clientY: $foreign.clientY, 
    ctrlKey: $foreign.ctrlKey, 
    getModifierState: $foreign.getModifierState, 
    metaKey: $foreign.metaKey, 
    screenX: $foreign.screenX, 
    screenY: $foreign.screenY, 
    shiftKey: $foreign.shiftKey
};
