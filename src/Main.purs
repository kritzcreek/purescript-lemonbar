module Main where

import Color (black)
import Color.Scheme.MaterialDesign as MD
import Control.Apply (lift2, (<*))
import Control.Monad.Eff.Console (log)
import Lemon.Bar
import Lemon.Bar (Format(AlignCenter))
import Lemon.CPU (cpuSection)
import Lemon.Clock (clockSection)
import Node.ChildProcess (stderr, stdout, stdin)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (writeString, onDataString)
import Prelude
import Prelude (class Semigroup, (#), ($), unit, pure, void)
import Signal (constant, Signal, sampleOn, (~), (<~), (~>), runSignal)
import Signal.Time (second, every)

combine :: forall a. (Semigroup a) => Signal a -> Signal a -> Signal a
combine = lift2 (<>)

-- main :: forall e. Eff (console :: CONSOLE, cp :: CHILD_PROCESS, cpu :: CPU | e) Unit
main = do
  process ← runBar defaultBarCfg
  let lin = stdin process
      lout = stdout process
      lerr = stderr process

  onDataString lerr UTF8 log
  onDataString lout UTF8 log

  cpu <- cpuSection 1000 # applyFormat (FormattingBlock [BackgroundColor MD.red]) # applyFormat (FormattingBlock [AlignLeft]) 
  clock <- clockSection # applyFormat (FormattingBlock [BackgroundColor MD.green]) # applyFormat (FormattingBlock [AlignRight]) 

  let signal = sampleOn (every second) $ cpu `combine`
               constant (render (FormattingBlock [AlignCenter]) <> (render (FormattingBlock [BackgroundColor MD.cyan])) <> "  Whaddup? ") `combine`
               clock
  runSignal (signal ~> \s -> log s <* writeString lin UTF8 ( s <> "\n") (pure unit))
  log "Done."
