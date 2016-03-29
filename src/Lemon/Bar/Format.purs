module Lemon.Bar.Format where

import Color (toHexString, black, Color)
import Color.Scheme.MaterialDesign as MD
import Data.Foldable (foldMap)
import Data.Monoid (class Monoid)
import Lemon.Bar (Section)
import Prelude (map, pure, (<>), class Semigroup, show, bind)
import Signal ((~>))


newtype Formatting =
  Formatting
  -- { alignment :: Alignment
  { coloring :: Face
  , font :: Int
  }

overF g (Formatting f) = Formatting (g f)

defaultFormatting :: Formatting
defaultFormatting =
  Formatting
  -- { alignment: AlignLeft
  { coloring: defaultFace
  , font: 1
  }

data Face
  = Coloring
    { background :: Color
    , foreground :: Color
    }
  | SwapColors

defaultFace :: Face
defaultFace =
  Coloring
  { background: black
  , foreground: MD.red
  }

data Format
  -- = Align Alignment
  = FormatColor Face
  | Offset Int
  | Font Int

addFormat :: Format -> Formatting -> Formatting
addFormat (FormatColor c) f = _ {coloring=c} `overF` f
addFormat (Offset _) f      = f
addFormat (Font i) f        = _ {font=i} `overF` f

renderFormatting :: Formatting -> String
renderFormatting (Formatting f) = renderFace f.coloring <> renderFont f.font

renderFace :: Face -> String
renderFace (Coloring {background, foreground}) =
  bracket ("F" <> toHexString foreground) <>
  bracket ("B" <> toHexString background)
renderFace SwapColors = bracket "R"

renderFont :: Int -> String
renderFont i = bracket ("T" <> show i)

bracket :: String -> String
bracket s = "%{" <> s <> "}"

applyFormatting :: forall e. Formatting -> Section e -> Section e
applyFormatting f s = map (map (renderFormatting f <> _)) s

-- applyClickFormat :: forall e. String -> FormattingBlock -> Section e -> Section e
-- applyClickFormat c fb s = do
--   s' <- s
--   pure (s' ~> clickable c fb)

-- instance formattingBlockSemigroup :: Semigroup FormattingBlock where
--   append (FormattingBlock fb) (FormattingBlock fb') = FormattingBlock (fb <> fb')

-- instance formattingBlockMonoid :: Monoid FormattingBlock where
--   mempty = defaultFormat

-- clickable :: String -> FormattingBlock -> String -> String
-- clickable s fb section = render fb <> "%{A:" <> s <> ":}" <> section <> "{A}"

-- render :: FormattingBlock -> String
-- render (FormattingBlock f) = "%{" <> foldMap showFormat f <> "}"
