module Main where

import Prelude

import Coc.AppM (runAppM)
import Coc.Component.Routing as Routing
import Control.Coroutine as CR
import Control.Coroutine as Coroutine
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CoroutineAff
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.AVar as AVar
import Effect.Class (liftEffect)
import Effect.Console (log)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.PushState (makeInterface)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Event.PopStateEvent as HCE
import Web.HTML.Event.PopStateEvent.EventTypes as HCET
import Web.HTML.Location (pathname)
import Web.HTML.Window (location)
import Web.HTML.Window as Window

-- A producer coroutine that emits messages whenever the window emits a
-- `hashchange` event.
popStateProducer :: Coroutine.Producer HCE.PopStateEvent Aff Unit
popStateProducer = CoroutineAff.produce \emitter -> do
  listener <- eventListener (traverse_ (emit emitter) <<< HCE.fromEvent)
  liftEffect $
    window
      >>= Window.toEventTarget
      >>> addEventListener HCET.popstate listener false

-- A consumer coroutine that takes the `query` function from our component IO
-- record and sends `ChangeRoute` queries in when it receives inputs from the
-- producer.
popStateConsumer
  :: (forall a. Routing.Query a -> Aff (Maybe a))
  -> Coroutine.Consumer HCE.PopStateEvent Aff Unit
popStateConsumer query = CR.consumer \event -> do
  path <- liftEffect $ window >>= location >>= pathname
  H.liftEffect $ log path
  void $ query $ H.tell $ Routing.ChangeRoute path
  pure Nothing

main :: Effect Unit
main = HA.runHalogenAff do
  globalMessage <- AVar.empty
  pushStateInterface <- H.liftEffect $ makeInterface
  let
    environment = { globalMessage, pushStateInterface }
    component = H.hoist (runAppM environment) Routing.component
  body <- HA.awaitBody
  io <- runUI component unit body
  Coroutine.runProcess (popStateProducer Coroutine.$$ popStateConsumer io.query)
