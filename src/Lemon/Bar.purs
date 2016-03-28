module Lemon.Bar where

import Color (toHexString, black, Color)
import Color.Scheme.MaterialDesign as MD
import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Node.ChildProcess (defaultSpawnOptions, spawn, ChildProcess, CHILD_PROCESS)
import Prelude (show, (<>))
import Signal (Signal)

foreign import data LEMON ∷ !

type Section e = Eff e (Signal String)

type BarEff e = Eff (lemon ∷ LEMON, cp ∷ CHILD_PROCESS | e)
type BarConfig =
  { dimensions ∷ {width ∷ Int, height ∷ Int}
  , position ∷ {x ∷ Int, y ∷ Int }
  , font ∷ Array String
  , backgroundColor ∷ Color
  , foregroundColor ∷ Color
  }

defaultBarCfg ∷ BarConfig
defaultBarCfg = { dimensions: {width: 1200, height: 50}
                , position: {x: 300, y: 50 }
                , font: ["Operator Mono", "Liberation Mono"]
                , backgroundColor: MD.red
                , foregroundColor: black
                }

mkOpts ∷ BarConfig → Array String
mkOpts {dimensions: {width, height}, position: {x, y}, font, backgroundColor, foregroundColor} =
  [ "-g", show width <> "x" <> show height <> "+" <> show x <> "+" <> show y
  , "-B", toHexString backgroundColor
  , "-F", toHexString foregroundColor
  , "-f"
  ] <> font

runBar ∷ ∀ e. BarConfig → BarEff e ChildProcess
runBar bc = spawn "lemonbar" ("-p" : (mkOpts bc)) defaultSpawnOptions

