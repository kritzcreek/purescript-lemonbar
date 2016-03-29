module Lemon.Bar.Label where

import Prelude
import Signal (constant)
import Lemon.Bar (Section)

labelSection :: forall e. String -> Section e
labelSection s = pure (constant s)
