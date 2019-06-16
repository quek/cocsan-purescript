module Main where

import Prelude

import Coc.Component.Routing as Routing
import Control.Coroutine as CR
import Control.Coroutine as Coroutine
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CoroutineAff
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.HashChangeEvent as HCE
import Web.HTML.Event.HashChangeEvent.EventTypes as HCET
import Web.HTML.Window as Window

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
hashChangeProducer :: Coroutine.Producer HCE.HashChangeEvent Aff Unit
hashChangeProducer = CoroutineAff.produce \emitter -> do
  listener <- eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
  liftEffect $
    window
      >>= Window.toEventTarget
      >>> addEventListener HCET.hashchange listener false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
hashChangeConsumer
  :: (forall a. Routing.Query a -> Aff (Maybe a))
  -> Coroutine.Consumer HCE.HashChangeEvent Aff Unit
hashChangeConsumer query = CR.consumer \event -> do
  let hash = Str.drop 1 $ Str.dropWhile (_ /= '#') $ HCE.newURL event
  void $ query $ H.tell $ Routing.ChangeRoute hash
  pure Nothing

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  io <- runUI Routing.component unit body
  -- Connecting the consumer to the producer initializes both, adding the event
  -- listener to the window and feeding queries back to our component as events
  -- are received.
  Coroutine.runProcess (hashChangeProducer Coroutine.$$ hashChangeConsumer io.query)
