"use strict";

const firebase = require('firebase/app');
require('firebase/auth');
require('firebase/firestore');
require('firebase/storage');
import './css/main.scss';


fetch('/__/firebase/init.json')
  .then(response => {
    return response.json();
  })
  .then(config => {
    firebase.initializeApp(config);

    firebase.auth().onAuthStateChanged(function(user) {
      if (user) {
        require('./Main').main();
      } else {
        const provider = new firebase.auth.GoogleAuthProvider();
        firebase.auth().signInWithRedirect(provider);
      }
    });
  });
