import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)
import Window exposing (..)


type alias Dataset = List Float

main =
    normalize [10, 10] |> calcArcLength |> show

normalize dataset =
  List.map (\data -> data / List.sum(dataset)) dataset

calcArcLength: List Float -> List Float
calcArcLength normalizedData =
    List.map (\data -> data * 2 * pi - pi / 2) normalizedData

calcArcSegments: List Float -> List (Float,Float)
calcArcSegments arclengths=
  List.foldr (\data previous -> (0,0)) 0 arclengths
