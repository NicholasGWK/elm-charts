import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import List exposing (..)
import Window exposing (..)


main =
  show <| List.reverse <| calcArcLengths <| normalize <| [1,2]


normalize: List Float -> List Float
normalize dataset =
  List.map (\data -> data * pi / List.sum(dataset)) dataset

calcArcLengths: List Float -> List (Float,Float)
calcArcLengths normalizedData =
    foldl addToList [] normalizedData

addToList: Float -> List (Float,Float) -> List (Float,Float)
addToList x xs =
  case xs of
    [] ->
      [(0,x)]

    (v1,v2)::t1 -> (v2,v2 + x)::xs
