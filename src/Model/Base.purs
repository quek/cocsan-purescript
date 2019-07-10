module Coc.Model.Base where

import Coc.Firebase.Firestore as Firestore
import Data.DateTime.Foreign (DateTime)

type BaseData =
  ( createdAt :: DateTime
  , updatedAt :: DateTime
  )

type BaseDoc = (ref :: Firestore.DocumentReference | BaseData)
