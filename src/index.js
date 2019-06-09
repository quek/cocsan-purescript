"use strict";

const firebase = require('firebase/app');
require('firebase/auth');
require('firebase/firestore');
require('firebase/storage');

fetch('/__/firebase/init.json')
  .then(response => {
    return response.json();
  })
  .then(config => {
    firebase.initializeApp(config);
    require('./Main').main();
  });
