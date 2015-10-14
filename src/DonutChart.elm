module DonutChart (chart) where
{-| Donut Chart

This module takes a List of floats which are just values you want to float in donut form.
The values are then normalized to percentages, and converted into arc segments (startRadian, endRadian).
Arc segments are then used to calculate a bezier curve equation with appropriate control points.
The equation is then mapped with a number of steps (default 100) to produce a smooth curve which becomes
A polygon, aka segment of the donut.

#Definition
@docs chart

-}

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import List exposing (..)
import Color exposing (..)
import Debug exposing (..)

{-| Creates a donut chart
Restrictions: No value should be larger than 25% of the sum. There is a restriction in the bezier
approimation for angles > pi/2 radians, so I need to divide those up into multiple adjoining segments

Colors must be provided currently.

  chart [red, blue, green, yellow] [1,1,1,1] 50 100 == "Donut chart split in 4, with innerRadius 50 and outerRadus 100"
-}
chart: List Color -> List Float -> Float -> Float -> List Form
chart colors data innerRadius outerRadius =
  List.map2 (\color arc -> filled color (arcToPolygon arc innerRadius outerRadius)) colors (dataToArcSegements data)

dataToArcSegements: List Float -> List (Float,Float)
dataToArcSegements data =
  data |> normalize |> calcArcLengths

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

--Curried, so leaves t as an argument
arcSegmentToBezierCurveEquation: (Float,Float) -> Float -> Float -> (Float,Float)
arcSegmentToBezierCurveEquation arc radius =
 computeBezier ( fromPolar (radius, fst arc)) (fromPolar (computeControlPoint (radius, fst arc) (calculateC arc) (pi / 2)  radius)) (fromPolar(computeControlPoint (radius,snd arc) (calculateC arc) (pi / 2 * -1) radius)) (fromPolar(radius, snd arc))


scalePolar: (Float,Float) -> Float -> (Float,Float)
scalePolar point m =
  ((fst point) * m, (snd point) * m)

calculateC: (Float, Float) -> Float
calculateC arc =
 ((4/3) * tan (pi / (2*( ((2 * pi)/((snd arc) - fst (arc)) )))))

computeBezier: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Float -> (Float, Float)
computeBezier p0 p1 p2 p3 t =
   ( (1-t)^3 * fst p0 + 3 * (1-t)^2 * t * fst p1 + 3 * (1-t) * t^2 * fst p2 + t^3 * fst p3,
     (1-t)^3 * snd p0 + 3 * (1-t)^2 * t * snd p1 + 3 * (1-t) * t^2 * snd p2 + t^3 * snd p3)

steps: Float -> List Float
steps num =
  List.map (\current -> current / num) [0..num]

computeControlPoint: (Float,Float) -> Float -> Float -> Float -> (Float,Float)
computeControlPoint p0 c rotation radius =
  toPolar (addPolar p0 ((c*radius,((snd p0) + rotation))))

addPolar: (Float,Float) -> (Float,Float) -> (Float,Float)
addPolar p1 p2 =
  ( (fst (fromPolar p1)) + (fst (fromPolar p2)), (snd (fromPolar p1)) + (snd (fromPolar p2)))
