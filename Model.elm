module Model (..) where

import Color exposing (Color, rgb)
import Colors exposing (..)


type alias Piece =
  { points : List ( Float, Float )
  , position : ( Float, Float )
  , rotation : Float
  , color : Color
  , scale : Float
  }


type alias Model =
  { dimensions : ( Int, Int )
  , bigTriangleS : Piece
  , bigTriangleW : Piece
  , mediumTriangle : Piece
  , smallTriangleSE : Piece
  , smallTriangleN : Piece
  , square : Piece
  , parallelogram : Piece
  }


init : Model
init =
  Model
    ( 200, 200 )
    (Piece trianglePoints ( 0, -0.5 ) 0 elmTurquoise 1)
    (Piece trianglePoints ( -0.5, 0 ) (degrees -90) elmGray 1)
    (Piece trianglePoints ( 0.75, 0.75 ) (degrees -45) elmTurquoise (1 / sqrt 2))
    (Piece trianglePoints ( 0.75, -0.5 ) (degrees 90) elmOrange 0.5)
    (Piece trianglePoints ( 0, 0.25 ) (degrees 180) elmOrange 0.5)
    (Piece squarePoints ( 0.5, 0 ) 0 elmGreen 1)
    (Piece parallelogramPoints ( 0, 0 ) 0 elmGreen 1)


trianglePoints : List ( number, Float )
trianglePoints =
  [ ( 0, 0.5 ), ( -1, -0.5 ), ( 1, -0.5 ) ]


squarePoints : List ( Float, Float )
squarePoints =
  [ ( 0.5, 0 ), ( 0, 0.5 ), ( -0.5, 0 ), ( 0, -0.5 ) ]


parallelogramPoints : List ( Float, Float )
parallelogramPoints =
  [ ( 0, 1 ), ( -1, 1 ), ( -0.5, 0.5 ), ( 0.5, 0.5 ) ]
