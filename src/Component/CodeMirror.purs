module Coc.Component.CodeMirror where

import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn1, runEffectFn1)
import Prelude (Unit)

foreign import data CodeMirror :: Type

foreign import makeImpl :: EffectFn1 String CodeMirror
make :: String -> Effect CodeMirror
make = runEffectFn1 makeImpl

foreign import getValueImpl :: EffectFn1 CodeMirror String
getValue ::  CodeMirror -> Effect String
getValue = runEffectFn1 getValueImpl

foreign import onImpl :: forall ev a. Fn3 CodeMirror String (ev -> Effect a) (Effect Unit)
-- on :: forall a b. CodeMirror -> String -> (a -> Effect b) -> Effect Unit
-- on codeMirror eventName fn = runFn3 onImpl codeMirror eventName fn

onChange :: forall a b. CodeMirror -> (a -> Effect b) -> Effect Unit
onChange codeMirror fn = runFn3 onImpl codeMirror "change" fn
