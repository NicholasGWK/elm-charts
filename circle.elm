module Circle where
--Test to see if I can draw a circle using bezier curves!

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Debug exposing (..)


testdata = [1,1,1]

colors = [ red , blue , yellow , green , lightRed, lightGreen, lightYellow]

main =
  collage 200 200
  (dataToPolygons colors (calcArcLengths (normalize testdata)) 40 80)


dataToPolygons: List Color -> List (Float,Float) -> Float -> Float -> List Form
dataToPolygons colors data innerRadius outerRadius =
  List.map2 (\color arc -> filled color (arcToPolygon arc innerRadius outerRadius)) colors data


arcToPolygon: (Float, Float) -> Float -> Float -> Path
arcToPolygon arc innerradius outerradius=
  append (List.reverse (List.map (arcSegmentToBezierCurveEquation arc innerradius) (steps 100)))
  (List.map (arcSegmentToBezierCurveEquation arc outerradius) (steps 100))
  |> path


normalize: List Float -> List Float
normalize dataset =
    List.map (\data -> data * 2 * pi / List.sum(dataset)) dataset

calcArcLengths: List Float -> List (Float,Float)
calcArcLengths normalizedData =
      foldl addToList [] normalizedData

addToList: Float -> List (Float,Float) -> List (Float,Float)
addToList x xs =
    case xs of
      [] ->
        [(0,x)]

      (v1,v2)::t1 -> (v2,v2 + x)::xs


arcSegmentToBezierCurveEquation arc radius =
 computeBezier ( fromPolar (radius, fst arc)) (fromPolar (computeControlPoint (radius, fst arc) (calculateC arc) (pi / 2)  radius)) (fromPolar(computeControlPoint (radius,snd arc) (calculateC arc) (pi / 2 * -1) radius)) (fromPolar(radius, snd arc))


scalePolar: (Float,Float) -> Float -> (Float,Float)
scalePolar point m =
  ((fst point) * m, (snd point) * m)


calculateC arc =
 ((4/3) * tan (pi / (2*( ((2 * pi)/((snd arc) - fst (arc)) )))))

computeBezier p0 p1 p2 p3 t =
   ( (1-t)^3 * fst p0 + 3 * (1-t)^2 * t * fst p1 + 3 * (1-t) * t^2 * fst p2 + t^3 * fst p3,
     (1-t)^3 * snd p0 + 3 * (1-t)^2 * t * snd p1 + 3 * (1-t) * t^2 * snd p2 + t^3 * snd p3)

steps num =
  List.map (\current -> current / num) [0..num]

computeControlPoint: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
computeControlPoint p0 c rotation radius =
  toPolar (addPolar p0 ((c*radius,((snd p0) + rotation))))

addPolar: (Float,Float) -> (Float,Float) -> (Float,Float)
addPolar p1 p2 =
  ( (fst (fromPolar p1)) + (fst (fromPolar p2)), (snd (fromPolar p1)) + (snd (fromPolar p2)))
