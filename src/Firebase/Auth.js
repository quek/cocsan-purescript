"use strict";

const firebase = require('firebase/app');
require('firebase/auth');

exports.currentUser = function() {
  return firebase.auth().currentUser;
};

exports.uid = function(user) {
  return user.uid;
};
