module Coc.Store where

import Prelude

import Coc.Store.Collection as Collection
import Coc.Firebase.Auth as Auth
import Coc.Firebase.Firestore as Firestore
import Effect (Effect)

uid :: Effect String
uid = do
  user <- Auth.currentUser
  pure $ Auth.uid user

userDocument :: Effect Firestore.DocumentReference
userDocument = do
  uid' <- uid
  firestore <- Firestore.firestore
  firestore
    # Firestore.collection Collection.users
    # Firestore.doc uid'
    # pure

userNotes :: Effect Firestore.CollectionReference
userNotes = do
  doc <- userDocument
  doc
    # Firestore.collection Collection.notes
    # pure

userTasks :: Effect Firestore.CollectionReference
userTasks = do
  doc <- userDocument
  doc
    # Firestore.collection Collection.tasks
    # pure
