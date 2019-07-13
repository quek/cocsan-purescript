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
foreign import getImpl :: CollectionReference -> Effect (Promise QuerySnapshot)
get :: CollectionReference -> Aff QuerySnapshot
get collectionReference = liftEffect (getImpl collectionReference) >>= Promise.toAff

foreign import size :: QuerySnapshot -> Int
foreign import docs :: QuerySnapshot -> Array QueryDocumentSnapshot

foreign import documentDataImpl :: Fn1  QueryDocumentSnapshot DocumentData
documentData :: QueryDocumentSnapshot -> DocumentData
documentData = runFn1 documentDataImpl

foreign import getFieldImpl :: Fn2 QueryDocumentSnapshot String String
getField :: QueryDocumentSnapshot -> String -> String
getField = runFn2 getFieldImpl

foreign import ref :: QueryDocumentSnapshot -> DocumentReference

foreign import deleteImpl :: DocumentReference -> Effect (Promise Unit)
delete :: DocumentReference -> Aff Unit
delete documentReference = liftEffect (deleteImpl documentReference) >>= Promise.toAff
