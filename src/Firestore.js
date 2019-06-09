"use strict";
const firebase = require('firebase/app');
require('firebase/firestore');

exports.collection = function(collectionName) {
  return firebase.firestore().collection(collectionName);
};

exports.getImpl = function(collection) {
  return collection.get();
};
