module Coc.Model.Base where

import Coc.Firebase.Firestore as Firestore
import Coc.Model.DateTime (DateTime)
import Data.Generic.Rep (class Generic)
import Foreign (Foreign)
import Foreign.Generic (defaultOptions, genericEncode)
import Foreign.Generic.Class (class GenericEncode)

type BaseData =
  ( createdAt :: DateTime
  , updatedAt :: DateTime
  )

type BaseDoc = (ref :: Firestore.DocumentReference | BaseData)


encode :: forall a rep. Generic a rep => GenericEncode rep => a -> Foreign
encode a = genericEncode defaultOptions a
