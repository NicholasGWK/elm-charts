module Circle where
--Test to see if I can draw a circle using bezier curves!

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)


main =
  collage 200 200 [
  (traced (solid red) (List.map (arcSegmentToBezierCurve (0,pi/2) 100) (steps 100)))
  ]


arcSegmentToBezierCurve arc radius =
  computeBezier ( fromPolar (radius, fst arc)) (fromPolar (computeControlPoint (radius, fst arc) (pi / 2) )) (fromPolar(computeControlPoint (radius,snd arc) (pi / 2 * -1)))   (fromPolar(radius, snd arc))


scalePolar: (Float,Float) -> Float -> (Float,Float)
scalePolar point m =
  ((fst point) * m, (snd point) * m)

c = 0.551915024494 --Optimal distance to control point. Must be rotated 90/-90 from start/end point of arc segment

computeBezier p0 p1 p2 p3 t =
   ( (1-t)^3 * fst p0 + 3 * (1-t)^2 * t * fst p1 + 3 * (1-t) * t^2 * fst p2 + t^3 * fst p3,
     (1-t)^3 * snd p0 + 3 * (1-t)^2 * t * snd p1 + 3 * (1-t) * t^2 * snd p2 + t^3 * snd p3)

steps num =
  List.map (\current -> current / num) [0..num]

computeControlPoint: (Float,Float) -> Float -> (Float,Float)
computeControlPoint p0 rotation =
  toPolar (addPolar p0 (c*100,((snd p0) + rotation)))

addPolar: (Float,Float) -> (Float,Float) -> (Float,Float)
addPolar p1 p2 =
  ( (fst (fromPolar p1)) + (fst (fromPolar p2)), (snd (fromPolar p1)) + (snd (fromPolar p2)))
