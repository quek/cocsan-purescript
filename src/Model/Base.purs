module Coc.Model.Base where

import Prelude

import Coc.Firebase.Firestore as Firestore
import Coc.Model.DateTime (DateTime)
import Control.Monad.Except (runExcept)
import Data.Either (hush)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust)
import Foreign (Foreign)
import Foreign.Generic (defaultOptions, genericDecode, genericEncode)
import Foreign.Generic.Class (class GenericDecode, class GenericEncode)
import Partial.Unsafe (unsafePartial)

type BaseData =
  ( createdAt :: DateTime
  , updatedAt :: DateTime
  )

type BaseDoc = (ref :: Firestore.DocumentReference | BaseData)


encode :: forall a rep. Generic a rep => GenericEncode rep => a -> Foreign
encode a = genericEncode defaultOptions a

decode :: forall a rep. Generic a rep => GenericDecode rep => Foreign -> a
decode a = do
  let opts = defaultOptions {unwrapSingleConstructors = true}
  -- TODO エラー処理
  let maybeDoc = hush $ runExcept $ genericDecode opts a
  unsafePartial fromJust maybeDoc
