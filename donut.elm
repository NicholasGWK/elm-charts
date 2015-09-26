import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)
import Window exposing (..)


type alias Dataset = List Float


first = normalize [10, 10] |> calcArcLength
second = first |> calcDifferences
radius = 10

segments = makeTuples second first
main =


normalize dataset =
  List.map (\data -> data / List.sum(dataset)) dataset

calcArcLength: List Float -> List Float
calcArcLength normalizedData =
    List.map (\data -> data * 2 * pi - pi / 2) normalizedData


calcDifferences: List Float -> List Float
calcDifferences normalizedData =
  0 :: drop 1 normalizedData


makeTuples first second =
  List.map2 (\start end -> (start,start+end)) first second



  
