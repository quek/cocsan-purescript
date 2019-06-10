module Coc.Firestore where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import data CollectionReference :: Type
foreign import data QuerySnapshot :: Type
foreign import data QueryDocumentSnapshot :: Type
foreign import data DocumentData :: Type

foreign import collection :: String -> CollectionReference
-- collection :: String -> CollectionReference
-- collection = collectionImpl

foreign import id :: CollectionReference -> String

-- foreign import getImpl :: EffectFn1 CollectionReference QuerySnapshot
-- get :: CollectionReference -> Effect QuerySnapshot
-- get = runEffectFn1 getImpl
foreign import getImpl :: CollectionReference -> Effect (Promise QuerySnapshot)
get :: CollectionReference -> Aff QuerySnapshot
get collectionReference = liftEffect (getImpl collectionReference) >>= Promise.toAff

foreign import size :: QuerySnapshot -> Int

foreign import docs :: QuerySnapshot -> Array QueryDocumentSnapshot

foreign import documentDataImpl :: EffectFn1  QueryDocumentSnapshot DocumentData
documentData :: QueryDocumentSnapshot -> Effect DocumentData
documentData = runEffectFn1 documentDataImpl

foreign import getFieldImpl :: Fn2 QueryDocumentSnapshot String String
getField :: QueryDocumentSnapshot -> String -> String
getField = runFn2 getFieldImpl
