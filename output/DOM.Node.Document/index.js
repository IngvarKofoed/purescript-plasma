// Generated by purs version 0.11.4
"use strict";
var $foreign = require("./foreign");
var Control_Monad_Eff = require("../Control.Monad.Eff");
var Control_Semigroupoid = require("../Control.Semigroupoid");
var DOM = require("../DOM");
var DOM_Node_Types = require("../DOM.Node.Types");
var Data_Functor = require("../Data.Functor");
var Data_Maybe = require("../Data.Maybe");
var Data_Nullable = require("../Data.Nullable");
var Prelude = require("../Prelude");
var getElementsByTagNameNS = function ($0) {
    return $foreign._getElementsByTagNameNS(Data_Nullable.toNullable($0));
};
var documentElement = function ($1) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._documentElement($1));
};
var doctype = function ($2) {
    return Data_Functor.map(Control_Monad_Eff.functorEff)(Data_Nullable.toMaybe)($foreign._doctype($2));
};
var createElementNS = function ($3) {
    return $foreign._createElementNS(Data_Nullable.toNullable($3));
};
module.exports = {
    createElementNS: createElementNS, 
    doctype: doctype, 
    documentElement: documentElement, 
    getElementsByTagNameNS: getElementsByTagNameNS, 
    adoptNode: $foreign.adoptNode, 
    characterSet: $foreign.characterSet, 
    compatMode: $foreign.compatMode, 
    contentType: $foreign.contentType, 
    createComment: $foreign.createComment, 
    createDocumentFragment: $foreign.createDocumentFragment, 
    createElement: $foreign.createElement, 
    createProcessingInstruction: $foreign.createProcessingInstruction, 
    createTextNode: $foreign.createTextNode, 
    documentURI: $foreign.documentURI, 
    getElementsByClassName: $foreign.getElementsByClassName, 
    getElementsByTagName: $foreign.getElementsByTagName, 
    importNode: $foreign.importNode, 
    origin: $foreign.origin, 
    url: $foreign.url
};
