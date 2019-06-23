"use strict";

const firebase = require('firebase/app');
require('firebase/firestore');

exports.collection = function(collectionName) {
  return firebase.firestore().collection(collectionName);
};

exports.docImpl = function(documentPath, collectionReference) {
  return collectionReference.doc(documentPath);
};

exports.subCollectionImpl = function(collectionPath, documentReference) {
  return documentReference.collection(collectionPath);
};

exports.addImpl = function(data, collectionReference) {
  // Promise<DocumentReference>
  return collectionReference.add(data.contents);
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

exports.docs = function(querySnapshot) {
  return querySnapshot.docs;
};

exports.documentDataImpl = function(querySnapshot) {
  return querySnapshot.data();
};

exports.getFieldImpl = function(queryDocumentSnapshot, fieldPath) {
  return queryDocumentSnapshot.get(fieldPath);
};

exports.ref = function(documeentSnapshot) {
  return documeentSnapshot.ref
};