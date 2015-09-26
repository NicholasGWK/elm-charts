--Test to see if I can draw a circle using bezier curves!

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)

main =
   collage 200 200 [ traced (solid red) curve]

c = 0.551915024494

computerBezier p0 p1 p2 p3 t =
  (1-t)^3 * p0 + 3 * (1-t)^2 * t * p1 + 3 * (1-t) * t^2 * p2 + t^3 * p3

steps num =
  List.map (\current -> current / num) [0..num]


half p0 p1 p2 p3 num =
  List.map(\step -> 100 * (computerBezier p0 p1 p2 p3 step)) (steps num)

curve =
  (List.map2 (\first second -> (first,second)) (half 0 c 1 1 100) (half 1 1 c 0 100))
