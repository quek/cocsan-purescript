module Coc.Component.EditorComponent where

import Prelude

import Coc.AppM (class LogMessages)
import Coc.Component.CodeMirror as CodeMirror
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES

type Slot = H.Slot Query Output

data Query a = ChangeText String a

data Output = TextChanged String

data Action
  = Initialize
  | Finalize
  | HandleChange

type State = { editor :: Maybe CodeMirror.CodeMirror
             , body :: String
             }

component :: forall m
             . MonadAff m
             => LogMessages m
             => H.Component HH.HTML Query String Output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval
        { handleAction = handleAction
        , handleQuery = handleQuery
        , initialize = Just Initialize
        , finalize = Just Finalize
        }
    }

initialState :: String -> State
initialState body = { editor: Nothing, body }

render :: forall m. State -> H.ComponentHTML Action () m
render
  = const $ HH.textarea [ HP.id_ "editor" ]

handleAction :: forall m
             . MonadAff m
             => LogMessages m
             => Action -> H.HalogenM State Action () Output m Unit
handleAction = case _ of
  Initialize -> do
    state <- H.get
    editor <- H.liftEffect $ CodeMirror.make "editor"
    H.liftEffect $ CodeMirror.setValue editor state.body
    H.modify_ (_ { editor = Just editor })
    void $ H.subscribe $ ES.effectEventSource \emitter -> do
      CodeMirror.onChange editor (\_ -> ES.emit emitter HandleChange)
      pure mempty
  Finalize -> do
    H.modify_ (_ { editor = Nothing })
  HandleChange -> do
    H.gets _.editor >>= traverse_ \editor -> do
      text <- H.liftEffect $ CodeMirror.getValue editor
      H.raise $ TextChanged text

handleQuery :: forall m a. MonadAff m => Query a -> H.HalogenM State Action () Output m (Maybe a)
handleQuery = case _ of
  ChangeText text next -> do
    maybeEditor <- H.gets _.editor
    case maybeEditor of
      Nothing -> pure unit
      Just editor -> do
        -- current <- H.liftEffect $ Editor.getValue editor
        -- when (text /= current) do
        --   void $ H.liftEffect $ Editor.setValue text Nothing editor
        pure unit
    H.raise $ TextChanged text
    pure (Just next)
