module Coc.Component.CodeMirror where

import Prelude

foreign import data CodeMirror :: Type

foreign import make :: String -> CodeMirror
