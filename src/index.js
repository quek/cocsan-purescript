"use strict";

import firebase from 'firebase/app';
import 'firebase/auth';
import 'firebase/firestore';
import 'firebase/storage';
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
