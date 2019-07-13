module Coc.Firebase.Firestore where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, Fn1, runFn1, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn2, runEffectFn2)
import Effect.Class (liftEffect)
import Foreign (Foreign)

foreign import data Firestore :: Type
foreign import data CollectionReference :: Type
foreign import data QuerySnapshot :: Type
foreign import data QueryDocumentSnapshot :: Type
foreign import data DocumentSnapshot :: Type
type DocumentData = Foreign
foreign import data DocumentReference :: Type

foreign import firestore :: Effect Firestore

foreign import collectionImpl :: forall a. Fn2 String a CollectionReference

class HasCollection a where
  collection :: String -> a -> CollectionReference

instance hasCollectionFirestore :: HasCollection Firestore where
  collection = runFn2 collectionImpl

instance hasCollectionDocumentReference :: HasCollection DocumentReference where
  collection = runFn2 collectionImpl

foreign import docImpl :: Fn2 String CollectionReference DocumentReference
doc :: String -> CollectionReference -> DocumentReference
doc = runFn2 docImpl

foreign import addImpl :: EffectFn2 DocumentData CollectionReference (Promise DocumentReference)
-- addImpl' :: DocumentData -> CollectionReference -> Effect (Promise DocumentReference)
-- addImpl' = runEffectFn2 addImpl
-- add :: DocumentData -> CollectionReference -> Aff DocumentReference
-- add d c = liftEffect (addImpl' d c) >>= Promise.toAff
add :: DocumentData -> CollectionReference -> Aff DocumentReference
add d c = liftEffect (runEffectFn2 addImpl d c) >>= Promise.toAff

foreign import idImpl :: forall a. a -> String

class HasId a where
  id :: a -> String

instance hasIdCollectionReference :: HasId CollectionReference where
  id = idImpl

instance hasIdDocumentReference :: HasId DocumentReference where
  id = idImpl


-- foreign import getImpl :: EffectFn1 CollectionReference QuerySnapshot
-- get :: CollectionReference -> Effect QuerySnapshot
-- get = runEffectFn1 getImpl
foreign import getImpl :: forall a b. a -> Effect (Promise b)
getImpl' :: forall a b. a -> Aff b
getImpl' a = liftEffect (getImpl a) >>= Promise.toAff

class Get a b where
  get :: a -> b

instance getCollectionReference :: Get CollectionReference (Aff QuerySnapshot) where
  get = getImpl'

instance getDocumentReference :: Get DocumentReference (Aff DocumentSnapshot) where
  get = getImpl'

foreign import size :: QuerySnapshot -> Int
foreign import docs :: QuerySnapshot -> Array QueryDocumentSnapshot

foreign import dataImpl :: forall a. Fn1 a DocumentData
dataImpl' :: forall a. a -> DocumentData
dataImpl' = runFn1 dataImpl
foreign import refImpl :: forall a. a -> DocumentReference

class Snapshot a where
  data' :: a -> DocumentData
  ref :: a -> DocumentReference

instance dataDocumentSnapshot :: Snapshot DocumentSnapshot where
  data' = dataImpl'
  ref = refImpl

instance dataQueryDocumentSnapshot :: Snapshot QueryDocumentSnapshot where
  data' = dataImpl'
  ref = refImpl

foreign import getFieldImpl :: Fn2 QueryDocumentSnapshot String String
getField :: QueryDocumentSnapshot -> String -> String
getField = runFn2 getFieldImpl

foreign import deleteImpl :: DocumentReference -> Effect (Promise Unit)
delete :: DocumentReference -> Aff Unit
delete documentReference = liftEffect (deleteImpl documentReference) >>= Promise.toAff

foreign import updateImpl :: EffectFn2 DocumentData DocumentReference (Promise Unit)
update :: DocumentData -> DocumentReference -> Aff Unit
update documentData documentReference =
  liftEffect (runEffectFn2 updateImpl documentData documentReference) >>= Promise.toAff
