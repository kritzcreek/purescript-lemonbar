module Lemon.Bar where

import Color (black, toHexString, Color)
import Color.Scheme.MaterialDesign as MD
import Control.Apply (lift2)
import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Data.Foldable (foldl)
import Data.Traversable (sequence)
import Node.ChildProcess (defaultSpawnOptions, spawn, ChildProcess, CHILD_PROCESS)
import Prelude
import Signal (constant, Signal)

clickable :: forall a e. String -> Section e -> Section e
clickable a s =
  (\s' -> constant ("%{A:" <> a <> ":}") <+> s' <+> constant "%{A}") <$> s

foreign import data LEMON ∷ !

type Section e = Eff e (Signal String)

type BarEff e = Eff (lemon ∷ LEMON, cp ∷ CHILD_PROCESS | e)
type BarConfig =
  { dimensions ∷ {width ∷ Int, height ∷ Int}
  , position ∷ {x ∷ Int, y ∷ Int }
  , fonts ∷ Array String
  , backgroundColor ∷ Color
  , foregroundColor ∷ Color
  }

newtype Lemonbar e =
  Lemonbar
  { left :: Array (Section e)
  , center :: Array (Section e)
  , right :: Array (Section e)
  }

data Alignment
  = AlignLeft
  | AlignCenter
  | AlignRight

combine :: forall a. (Semigroup a) => Signal a -> Signal a -> Signal a
combine = lift2 (<>)

infixl 3 combine as <+>

mkBar :: forall e. Lemonbar e -> Eff e (Signal String)
mkBar (Lemonbar {left, center, right}) = do
  ls <- concatSection left
  cs <- concatSection center
  rs <- concatSection right
  pure $ constant lMarker <+> ls <+> constant cMarker <+> cs <+> constant rMarker <+> rs
  where
    concatSection :: forall eff. Array (Section eff) -> Eff eff (Signal String)
    concatSection = map (foldl (<+>) (constant "")) <<< sequence

lMarker:: String
lMarker = "%{l}"
rMarker :: String
rMarker = "%{r}"
cMarker :: String
cMarker = "%{c}"

defaultBarCfg ∷ BarConfig
defaultBarCfg = { dimensions: {width: 1200, height: 50}
                , position: {x: 300, y: 50 }
                , fonts: ["Operator Mono", "Liberation Mono"]
                , backgroundColor: MD.red
                , foregroundColor: black
                }

mkOpts ∷ BarConfig → Array String
mkOpts {dimensions: {width, height}, position: {x, y}, fonts, backgroundColor, foregroundColor} =
  [ "-g", show width <> "x" <> show height <> "+" <> show x <> "+" <> show y
  , "-B", toHexString backgroundColor
  , "-F", toHexString foregroundColor
  ] <> map ("-f" <> _) fonts

runBar ∷ ∀ e. BarConfig → BarEff e ChildProcess
runBar bc = spawn "lemonbar" ("-p" : "d" : (mkOpts bc)) defaultSpawnOptions

