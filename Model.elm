module Model (..) where

import Color exposing (Color, rgb)
import Colors exposing (..)
import Time exposing (Time)


type alias Position =
  ( Float, Float )


type alias Rotation =
  Float


{-| Each Piece is one of the 7 tangram pieces.
-}
type alias Piece =
  { points : List Position
  , position : Position
  , rotation : Rotation
  , color : Color
  , scale : Float
  }


{-| A Tangram comprises all 7 pieces.
-}
type alias Tangram =
  { bigTriangle1 : Piece
  , bigTriangle2 : Piece
  , mediumTriangle : Piece
  , smallTriangle1 : Piece
  , smallTriangle2 : Piece
  , square : Piece
  , parallelogram : Piece
  }


type alias Model =
  { dimensions : ( Int, Int )
  , tangram : Tangram
  , animation : Animation
  }


type Animation
  = AnimationIdle
  | AnimationStarting Time
  | AnimationActive { duration : Time, startTime : Time, startTangram : Tangram }


{-| The elmTangram has the pieces in the canonical Elm square shape, centered
at position 0,0 and having height and width of 2.
-}
elmTangram =
  Tangram
    (Piece trianglePoints ( 0, -0.5 ) 0 elmTurquoise 1)
    (Piece trianglePoints ( -0.5, 0 ) (degrees -90) elmGray 1)
    (Piece trianglePoints ( 0.75, 0.75 ) (degrees -45) elmTurquoise (1 / sqrt 2))
    (Piece trianglePoints ( 0.75, -0.5 ) (degrees 90) elmOrange 0.5)
    (Piece trianglePoints ( 0, 0.25 ) (degrees 180) elmOrange 0.5)
    (Piece squarePoints ( 0.5, 0 ) 0 elmGreen 1)
    (Piece parallelogramPoints ( -0.25, 0.75 ) 0 elmGreen 1)


addPosition : Position -> Position -> Position
addPosition ( x1, y1 ) ( x2, y2 ) =
  ( x1 + x2, y1 + y2 )


movePiece : Position -> Rotation -> Piece -> Piece
movePiece dPosition dRotation piece =
  { piece
    | position = addPosition piece.position dPosition
    , rotation = piece.rotation + (degrees dRotation)
  }


{-| Define the runner tangram shape relative to the base Elm tangram shape.
-}
runnerTangram =
  { elmTangram
    | bigTriangle1 = elmTangram.bigTriangle1 |> movePiece ( 0, 0.25 ) 0
    , bigTriangle2 = elmTangram.bigTriangle2 |> movePiece ( 1, 0.25 ) -90
    , mediumTriangle = elmTangram.mediumTriangle |> movePiece ( 0, -1.75 ) 0
    , smallTriangle1 = elmTangram.smallTriangle1 |> movePiece ( -0.1, -1.25 ) 45
    , smallTriangle2 = elmTangram.smallTriangle2 |> movePiece ( (1 - 2 * sqrt 2), -1.5 ) -135
    , square = elmTangram.square |> movePiece ( 0, 1.25 ) 360
    , parallelogram = elmTangram.parallelogram |> movePiece ( -(1 / sqrt 2), -1.86 ) 45
  }


init : Model
init =
  Model
    ( 200, 200 )
    elmTangram
    AnimationIdle


{-| A right triangle with mid-point at (0,0), hypotenuse at bottom of unit length
-}
trianglePoints : List Position
trianglePoints =
  [ ( 0, 0.5 ), ( -1, -0.5 ), ( 1, -0.5 ) ]


{-| A square centered at (0,0) with sides of unit length.
-}
squarePoints : List Position
squarePoints =
  [ ( 0.5, 0 ), ( 0, 0.5 ), ( -0.5, 0 ), ( 0, -0.5 ) ]


{-| The parallelogram, centered at (0,0) and having unit length on long sides.
-}
parallelogramPoints : List Position
parallelogramPoints =
  [ ( 0.25, 0.25 ), ( -0.75, 0.25 ), ( -0.25, -0.25 ), ( 0.75, -0.25 ) ]
