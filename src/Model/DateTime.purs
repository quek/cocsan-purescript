module Coc.Model.DateTime where

import Prelude
import Effect.Unsafe (unsafePerformEffect)
import Data.DateTime as D
import Foreign (F, ForeignError(..), fail)
import Foreign.Class (class Encode, class Decode, decode, encode)
import Data.JSDate (fromDateTime, parse, toDateTime, toISOString)
import Data.Maybe (maybe')

newtype DateTime = DateTime D.DateTime

derive newtype instance eqDateTime :: Eq DateTime
derive newtype instance ordDateTime :: Ord DateTime
derive newtype instance boundedDateTime :: Bounded DateTime
derive newtype instance showDateTime :: Show DateTime

invalidDate :: String -> Unit -> F DateTime
invalidDate date _ = fail $ ForeignError $ "Invalid date: " <> date

instance decodeDateTime :: Decode DateTime where
    decode value = do
        dateString <- decode value
        let jsDate = unsafePerformEffect $ parse dateString
        maybe' (invalidDate dateString) (pure <<< DateTime) $ toDateTime jsDate

instance encodeDateTime :: Encode DateTime where
    encode (DateTime dt) = encode $ unsafePerformEffect $ toISOString $ fromDateTime dt
