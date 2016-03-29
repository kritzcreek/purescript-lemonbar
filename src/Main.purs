module Main where

import Color (black)
import Color.Scheme.MaterialDesign as MD
import Control.Apply (lift2, (<*))
import Control.Monad.Eff.Console (log)
import Lemon.Bar
import Lemon.Bar (clickable, mkBar, Lemonbar(Lemonbar))
import Lemon.Bar.Format (Format(FormatColor, Font), Face(SwapColors, Coloring), addFormat, defaultFormatting, applyFormatting, Formatting)
import Lemon.CPU (cpuSection)
import Lemon.Clock (clockSection)
import Lemon.Bar.Label (labelSection)
import Node.ChildProcess (stderr, stdout, stdin)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (writeString, onDataString)
import Prelude
import Signal (constant, Signal, sampleOn, (~), (<~), (~>), runSignal)
import Signal.Time (second, every)

myCpu = cpuSection 1000
        # applyFormatting (defaultFormatting
                           # addFormat (Font 2)
                           # addFormat (FormatColor SwapColors))
myClock = clockSection
          # applyFormatting defaultFormatting

myLabel = labelSection "   PURSBAR!   "
           # clickable "myLabel"
           # applyFormatting (defaultFormatting
                              # addFormat (FormatColor (Coloring { foreground: black
                                                                 , background: MD.green}))
                              # addFormat (Font 1))

myBar = Lemonbar { left: [myCpu]
                 , center: [myLabel]
                 , right: [myClock]
                 }

-- main :: forall e. Eff (console :: CONSOLE, cp :: CHILD_PROCESS, cpu :: CPU | e) Unit
main = do
  process ← runBar defaultBarCfg
  let lin = stdin process
      lout = stdout process
      lerr = stderr process

  onDataString lerr UTF8 log
  onDataString lout UTF8 log

  bar <- mkBar myBar
  let signal = sampleOn (every second) $ bar
  runSignal (signal ~> \s -> log s <* writeString lin UTF8 ( s <> "\n") (pure unit))
  log "Done."
