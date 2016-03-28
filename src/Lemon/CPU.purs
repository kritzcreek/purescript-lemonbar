module Lemon.CPU (cpuSection, CPU) where

import Control.Monad.ST (readSTRef, writeSTRef, newSTRef, runST)
import Data.Function.Eff (mkEffFn1, runEffFn2, EffFn1, EffFn2)
import Data.Int (toNumber)
import Lemon.Bar (Section)
import Prelude (pure, show, (<<<), void, Unit, bind)
import Signal ((~>), unwrap)
import Signal.Time (every)

foreign import data CPU ∷ !
foreign import setupCpuUsage :: ∀ e. EffFn2 e Int (EffFn1 e Int Unit) Unit

cpuSection :: forall eff. Int -> Section (cpu :: CPU | eff)
cpuSection interval = runST do
  currentCpuUsage ← newSTRef 0
  runEffFn2 setupCpuUsage 1000 (mkEffFn1 (void <<< writeSTRef currentCpuUsage))
  let states = every (toNumber interval) ~> \_ -> do
        usage <- readSTRef currentCpuUsage
        pure (show usage)
  unwrap states
