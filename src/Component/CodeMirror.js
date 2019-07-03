"use strict";

const CodeMirror = require('codemirror');
require('codemirror/lib/codemirror.css');

exports.make = function(textAreaId) {
  return CodeMirror.fromTextArea(document.getElementById(textAreaId));
}
