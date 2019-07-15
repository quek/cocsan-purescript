module Coc.Model where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (decode)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Generic.Class (class GenericDecode)

class Model a where
  collection' :: forall f. f a -> Effect Firestore.CollectionReference
  find' :: String -> Aff { ref :: Firestore.DocumentReference,  data :: a }

findImpl :: forall a rep
            . Generic a rep
            => GenericDecode rep
            => Effect Firestore.CollectionReference
            -> String
            -> Aff { ref :: Firestore.DocumentReference,  data :: a }
findImpl collection' id = do
  collection'' <- liftEffect collection'
  documentSnapshot <- (Firestore.get $
                       Firestore.doc id
                       collection'') :: Aff Firestore.DocumentSnapshot
  pure $ { ref: (Firestore.ref documentSnapshot), data: decode $ Firestore.data' documentSnapshot }
