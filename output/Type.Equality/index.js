// Generated by purs version 0.11.4
"use strict";
var TypeEquals = function (from, to) {
    this.from = from;
    this.to = to;
};
var to = function (dict) {
    return dict.to;
};
var refl = new TypeEquals(function (a) {
    return a;
}, function (a) {
    return a;
});
var from = function (dict) {
    return dict.from;
};
module.exports = {
    TypeEquals: TypeEquals, 
    from: from, 
    to: to, 
    refl: refl
};
