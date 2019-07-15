module Coc.Model where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model.Base (decode, insertRef)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Foreign.Generic.Class (class GenericDecode)

class Model a where
  collection' :: forall f. f a -> Effect Firestore.CollectionReference

class Find a where
  find' :: String -> Aff { ref :: Firestore.DocumentReference,  data :: a }

findImpl :: forall f a rep
            . Model a
            => Generic a rep
            => GenericDecode rep
            => f a -> String -> Aff { ref :: Firestore.DocumentReference,  data :: a }
findImpl a id = do
  collection'' <- liftEffect $ collection' a
  documentSnapshot <- (Firestore.get $
                       Firestore.doc id
                       collection'') :: Aff Firestore.DocumentSnapshot
  pure $ { ref: (Firestore.ref documentSnapshot), data: decode $ Firestore.data' documentSnapshot }

