{-# LANGUAGE FlexibleInstances #-}
module Color where

import Control.Applicative ((<$>), (<*>))
import Data.Monoid    (mappend)
import Data.Semigroup (Semigroup, (<>))
import Data.Vector    (Vector)
import Data.Word      (Word8)
import Test.QuickCheck.Arbitrary (Arbitrary(..))

-- | an RGB value
data RGB a = RGB
  { r :: a
  , g :: a
  , b :: a
  } deriving (Eq, Ord, Read, Show)

instance (Num a) => Semigroup (RGB a) where
  (RGB r0 g0 b0) <> (RGB r1 g1 b1) =
    RGB (r0 + r1) (g0 + g1) (b0 + b1)

instance Arbitrary (RGB Word8) where
  arbitrary = RGB <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary (RGB Double) where
  arbitrary = rgb_w2d <$> arbitrary

rgb_w2d :: RGB Word8 -> RGB Double
rgb_w2d (RGB r g b) = RGB (w2d r) (w2d g) (w2d b)
  where
    w2d w = (fromIntegral w / 255)

rgb_d2w :: RGB Double -> RGB Word8
rgb_d2w (RGB r g b) = RGB (d2w r) (d2w g) (d2w b)
  where
    d2w w = round (w * 255)

scaleRGB :: (Num a) => RGB a -> a -> RGB a
scaleRGB (RGB r g b) s = RGB (r*s) (g*s) (b*s)

-- | standard colors
blackRGB :: RGB Word8
blackRGB = RGB 0x00 0x00 0x00

redRGB :: RGB Word8
redRGB = RGB 0xff 0x00 0x00

greenRGB :: RGB Word8
greenRGB = RGB 0x00 0xff 0x00

blueRGB :: RGB Word8
blueRGB = RGB 0x00 0x00 0xff


-- | an HSL value
data HSL a = HSL
  { h :: a
  , s :: a
  , l :: a
  } deriving (Eq, Ord, Read, Show)

hsl2rgb :: (Ord a, Fractional a) =>
           HSL a
        -> RGB a
hsl2rgb (HSL _ 0 l) = RGB l l l
hsl2rgb (HSL h s l) =
  let t2 | l < 0.5   = l * (1 + s)
         | otherwise = (l + s) - (l * s)
      t1  = (2.0 * l) - t2
      hk  = h / 360.0
      t3r = case hk + (1/3) of
               n | n > 1.0   -> n - 1.0
                 | otherwise -> n
      t3g = hk
      t3b = case hk - (1/3) of
               n | n < 0     -> n + 1.0
                 | otherwise -> n
  in RGB { r = colorComponent t1 t2 t3r
         , g = colorComponent t1 t2 t3g
         , b = colorComponent t1 t2 t3b
         }
  where
    colorComponent t1 t2 t3
      | t3  < 1/6             = t1 + ((t2 - t1) * 6.0 * t3)
      | t3 >= 1/6 && t3 < 1/2 = t2
      | t3 >= 1/2 && t3 < 2/3 = (t1 + ((t2 - t1) * (2/3 - t3) * 6.0))
      | otherwise             = t1


rgb2hsl :: (Ord a, Eq a, Fractional a) =>
           RGB a
        -> HSL a
rgb2hsl (RGB r g b) =
  let max = maximum [r, g, b]
      min = minimum [r, g, b]
      l   = (min + max) / 2
      s   | min == max = 0.0
          | l   <= 0.5 = (max - min) / (max + min)
          | otherwise  = (max - min) / (2 - (max + min))
      h   | min == max = 0.0
          | max == r && g >= b = 60 * ((g - b) / (max - min))
          | max == r           = 360 + (60 * ((g - b) / (max - min)))
          | max == g           = 120 + (60 * ((b - r) / (max - min)))
          | otherwise          = 240 + (60 * ((r - g) / (max - min)))

  in HSL h s l

prop_1 :: RGB Word8 -> Bool
prop_1 rgb = rgb_d2w (rgb_w2d rgb) == rgb

prop_2 :: RGB Word8 -> Bool
prop_2 rgb = rgb_d2w (hsl2rgb (rgb2hsl (rgb_w2d rgb))) == rgb
