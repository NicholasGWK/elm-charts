import Donut exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)


main =
  show <| List.reverse <| calcArcLengths <| normalize <| [1,2]
