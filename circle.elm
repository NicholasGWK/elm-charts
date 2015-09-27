--Test to see if I can draw a circle using bezier curves!

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)



type alias Point =
  {
  x: Float,
  y: Float
  }

fromXY: Point -> (Float,Float)
fromXY point =
  (point.x,point.y)

fromTuple: (Float,Float) -> Point
fromTuple point =
  {
  x = fst point,
  y = snd point
  }


multTuple: (Float,Float) -> Float -> (Float,Float)
multTuple point m =
  ((fst point) * m, (snd point) * m)


main =
  collage 200 200 [append (arcCurve (0,1) (c,1) (1,c) (1,0) 100 100) (reverse (arcCurve  (0,1) (c,1) (1,c) (1,0) 100 50))
  |> polygon
  |> filled red]

c = 0.551915024494

computerBezier p0 p1 p2 p3 t =
   ( (1-t)^3 * fst p0 + 3 * (1-t)^2 * t * fst p1 + 3 * (1-t) * t^2 * fst p2 + t^3 * fst p3,
     (1-t)^3 * snd p0 + 3 * (1-t)^2 * t * snd p1 + 3 * (1-t) * t^2 * snd p2 + t^3 * snd p3)

steps num =
  List.map (\current -> current / num) [0..num]


arcCurve p0 p1 p2 p3 num r =
  List.map(\step -> multTuple (computerBezier p0 p1 p2 p3 step) r) (steps num)
