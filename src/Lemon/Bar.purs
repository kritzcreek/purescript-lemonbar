module Lemon.Bar where

import Color (black, toHexString, Color)
import Color.Scheme.MaterialDesign as MD
import Control.Monad.Eff (Eff)
import Data.Array ((:))
import Data.Foldable (foldMap)
import Data.Monoid (class Monoid)
import Node.ChildProcess (defaultSpawnOptions, spawn, ChildProcess, CHILD_PROCESS)
import Prelude
import Prelude (class Semigroup, (<<<), map)
import Signal ((~>), Signal)

newtype FormattingBlock = FormattingBlock (Array Format)

instance formattingBlockSemigroup :: Semigroup FormattingBlock where
  append (FormattingBlock fb) (FormattingBlock fb') = FormattingBlock (fb <> fb')

instance formattingBlockMonoid :: Monoid FormattingBlock where
  mempty = FormattingBlock []

clickable :: String -> FormattingBlock -> String -> String
clickable s fb section = render fb <> "%{A:" <> s <> ":}" <> section <> "{A}"

render :: FormattingBlock -> String
render (FormattingBlock f) = "%{" <> foldMap showFormat f <> "}"

showFormat :: Format -> String
showFormat SwapColors = "R"
showFormat AlignLeft = "l"
showFormat AlignCenter = "c"
showFormat AlignRight = "r"
showFormat (Offset o) = "O" <> show o
showFormat (BackgroundColor c) = "B" <> toHexString c
showFormat (ForegroundColor c) = "F" <> toHexString c
showFormat (Font ix) = "T" <> show ix

data Format
  = SwapColors
  | AlignLeft
  | AlignCenter
  | AlignRight
  | Offset Int
  | BackgroundColor Color
  | ForegroundColor Color
  | Font Int

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

defaultBarCfg ∷ BarConfig
defaultBarCfg = { dimensions: {width: 1200, height: 50}
                , position: {x: 300, y: 50 }
                , fonts: ["Operator Mono", "Liberation Mono"]
                , backgroundColor: MD.red
                , foregroundColor: black
                }

defaultFormat :: FormattingBlock
defaultFormat = FormattingBlock [BackgroundColor MD.red, ForegroundColor black]

applyFormat :: forall e. FormattingBlock -> Section e -> Section e
applyFormat fb s = do
  s' <- s
  pure (s' ~> (render fb <> _))

applyClickFormat :: forall e. String -> FormattingBlock -> Section e -> Section e
applyClickFormat c fb s = do
  s' <- s
  pure (s' ~> clickable c fb)

mkOpts ∷ BarConfig → Array String
mkOpts {dimensions: {width, height}, position: {x, y}, fonts, backgroundColor, foregroundColor} =
  [ "-g", show width <> "x" <> show height <> "+" <> show x <> "+" <> show y
  , "-B", toHexString backgroundColor
  , "-F", toHexString foregroundColor
  ] <> map ("-f" <> _) fonts

runBar ∷ ∀ e. BarConfig → BarEff e ChildProcess
runBar bc = spawn "lemonbar" ("-p" : "d" : (mkOpts bc)) defaultSpawnOptions

