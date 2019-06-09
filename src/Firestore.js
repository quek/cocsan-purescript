"use strict";

const firebase = require('firebase/app');
require('firebase/firestore');

exports.collection = function(collectionName) {
  return firebase.firestore().collection(collectionName);
};

exports.id = function(hasId) {
  return hasId.id;
};

exports.getImpl = function(collection) {
  return function() {
    return collection.get();
  };
};

exports.size = function(querySnapshot) {
  return querySnapshot.size;
};
