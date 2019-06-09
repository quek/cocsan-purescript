module Coc.Firestore where

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)

foreign import data CollectionReference :: Type
foreign import data QuerySnapshot :: Type

foreign import collection :: String -> CollectionReference
-- collection :: String -> CollectionReference
-- collection = collectionImpl

foreign import getImpl :: EffectFn1 CollectionReference QuerySnapshot
get :: CollectionReference -> Effect QuerySnapshot
get = runEffectFn1 getImpl
