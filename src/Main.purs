module Main where

import Control.Monad.Eff.Console (log)
import Lemon.Bar (defaultBarCfg, runBar)
import Lemon.CPU (cpuSection)
import Node.ChildProcess (stderr, stdout, stdin)
import Node.Encoding (Encoding(UTF8))
import Node.Stream (writeString, onDataString)
import Prelude
import Prelude (($), unit, pure, void)
import Signal ((~>), runSignal)

-- wrapSection s = "%{ " <> s <> " }"

-- main :: forall e. Eff (console :: CONSOLE, cp :: CHILD_PROCESS, cpu :: CPU | e) Unit
main = do
  process ← runBar defaultBarCfg
  let lin = stdin process
      lout = stdout process
      lerr = stderr process

  onDataString lerr UTF8 log
  onDataString lout UTF8 log

  cpu <- cpuSection 1000

  runSignal (cpu ~> \s -> void $ writeString lin UTF8 ( s <> "\n") (pure unit))
  log "Done."
