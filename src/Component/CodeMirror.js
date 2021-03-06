"use strict";

const CodeMirror = require('codemirror');
require('codemirror/lib/codemirror.css');

exports.makeImpl = function(textAreaId) {
  return CodeMirror.fromTextArea(document.getElementById(textAreaId));
};

exports.getValueImpl = function(codeMirror) {
  return codeMirror.getValue();
};

exports.setValueImpl = function(codeMirror, value) {
  console.log(codeMirror);
  console.log(value);
  return codeMirror.setValue(value);
};

exports.onImpl = function(codeMirror, eventName, fn) {
  return function () {
    return codeMirror.on(eventName, function (ev) {
      fn(ev)();
    });
  };
};

