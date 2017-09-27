module Loss where

squared :: (Double -> Double -> Double) -> Double -> Double -> Double
squared f x y = let r = f x y in r * r
