module Temperature (kelvinToRGB, kelvinToColor) where
{-| This library handle something around temperatures.

# Kelvin to Color
@docs kelvinToRGB, kelvinToColor

-}

import Color exposing (Color, rgba)

pair3 : (a -> b) -> (a, a, a) -> (b, b, b)
pair3 f (x, y, z) = (f x, f y, f z)

pow : Float -> Float -> Float
pow x y = e ^ (y * logBase e x)

{-| calculate RGB from Kelvin

  kelvinToRGB 3000 == (255, 128, 43)
-}
kelvinToRGB : Float -> (Int, Int, Int)
kelvinToRGB t =
    let 
        (h, k, c) = (6.6260755e-34, 1.380658e-23, 2.99792458e+8)
        i x = (8 * pi * h * c) / (pow x 5 * (e ^ (h * c / (k * t * x)) - 1))
        (ir, ig, ib) = (i 0.7e-6, i 0.546e-6, i 0.436e-6)
        (r, g, b) = (ir/ig, 1.0, ib/ig)
        norm base = pair3 (\clr -> round ((clr / base) * 255))
        unnorm = pair3 (\clr -> round (clr * 255))
    in
      if r > 1.0  then norm r (r, g, b)
      else if b > 1.0 then norm b (r, g, b)
           else unnorm (r, g, b)

{-| calculate RGB from Kelvin and alpha

  kelvinToColor 3000 0.8 == RGBA 255 128 43 0.8
-}
kelvinToColor : Float -> Float -> Color
kelvinToColor t a = let (r, g, b) = kelvinToRGB t
                    in rgba r g b a
