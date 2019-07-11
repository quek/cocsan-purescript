module Coc.Model.DateTime where

import Prelude


import Data.DateTime as D

import Data.JSDate (JSDate, fromDateTime, toDateTime)
import Data.Maybe (maybe')
import Foreign (Foreign, ForeignError(..), fail, unsafeFromForeign, unsafeToForeign)
import Foreign.Class (class Decode, class Encode)


newtype DateTime = DateTime D.DateTime

derive newtype instance eqDateTime :: Eq DateTime
derive newtype instance ordDateTime :: Ord DateTime
derive newtype instance boundedDateTime :: Bounded DateTime
derive newtype instance showDateTime :: Show DateTime

foreign import toDate :: Foreign -> JSDate

instance decodeDateTime :: Decode DateTime where
    decode value = do
      let dateTime = toDateTime $ toDate $ unsafeFromForeign value
      maybe'
        (\_ -> fail $ ForeignError $ show dateTime)
        (pure <<< DateTime)
        dateTime

instance encodeDateTime :: Encode DateTime where
    encode (DateTime dt) = unsafeToForeign $ fromDateTime dt
