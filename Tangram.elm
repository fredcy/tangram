module Main (..) where

import Window
import Color exposing (Color, rgb)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Colors exposing (..)


--shapes


scalingFactor : ( Int, Int ) -> Float
scalingFactor ( width, height ) =
  (min (toFloat width) (toFloat height)) / 3


filledForm : Color -> List ( Float, Float ) -> Form
filledForm color points =
  let
    linestyle =
      { defaultLine | width = separation, color = white, join = Graphics.Collage.Smooth }

    -- Add first point to end of list so path forms a loop.
    loopedPoints =
      case List.head points of
        Nothing ->
          []

        Just h ->
          List.append points [ h ]
  in
    group
      [ polygon points |> filled color
      , path loopedPoints |> traced linestyle
      ]


separation : Float
separation =
  5.0e-2


logo : Float -> Form
logo scaleFactor =
  group
    [ bigTriangleTurquoise
    , bigTriangleGray
    , mediumTriangle
    , smallTriangleSE
    , smallTriangleN
    , square
    , parallelogram
    ]
    |> scale scaleFactor


view : Model -> Form
view model =
  group
    [ formFromPiece model.bigTriangleS
    , formFromPiece model.bigTriangleW
    ]
    |> scale (scalingFactor model.dimensions)


formFromPiece : Piece -> Form
formFromPiece piece =
  filledForm piece.color piece.points
    |> move piece.position
    |> rotate piece.rotation
    |> scale piece.scale


type alias Model =
  { dimensions : (Int, Int)
  , bigTriangleS : Piece
  , bigTriangleW : Piece
  }


type alias Piece =
  { points : List ( Float, Float )
  , position : ( Float, Float )
  , rotation : Float
  , color : Color
  , scale : Float
  }


init : Model
init =
  Model
    (200, 200)
    (Piece trianglePoints ( 0, -0.5 ) 0 elmTurquoise 1)
    (Piece trianglePoints ( -0.5, 0 ) (degrees -90) elmGray 1)


trianglePoints : List ( number, Float )
trianglePoints =
  [ ( 0, 0.5 ), ( -1, -0.5 ), ( 1, -0.5 ) ]


bigTriangleTurquoise : Form
bigTriangleTurquoise =
  filledForm elmTurquoise trianglePoints
    |> moveY -0.5


bigTriangleGray : Form
bigTriangleGray =
  filledForm elmGray trianglePoints
    |> rotate (degrees (-90))
    |> moveX -0.5


mediumTriangle : Form
mediumTriangle =
  filledForm elmTurquoise trianglePoints
    |> scale (1 / sqrt 2)
    |> rotate (degrees -45)
    |> move ( 0.75, 0.75 )


smallTriangleSE : Form
smallTriangleSE =
  filledForm elmOrange [ ( 1, -1 ), ( 1, 0 ), ( 0.5, -0.5 ) ]


smallTriangleN : Form
smallTriangleN =
  filledForm elmOrange [ ( 0, 0 ), ( 0.5, 0.5 ), ( -0.5, 0.5 ) ]


square : Form
square =
  filledForm elmGreen [ ( 0, 0 ), ( 0.5, 0.5 ), ( 1, 0 ), ( 0.5, -0.5 ) ]


parallelogram : Form
parallelogram =
  filledForm elmGreen [ ( 0, 1 ), ( -1, 1 ), ( -0.5, 0.5 ), ( 0.5, 0.5 ) ]


type Action
  = NoOp
  | UpdateDimensions (Int, Int)


update : Action -> Model -> Model
update action model =
  case action of
    UpdateDimensions dimensions ->
      { model | dimensions = dimensions }

    NoOp ->
      model



--functions


displayCentered : ( Int, Int ) -> Form -> Element
displayCentered ( width, height ) form =
  collage width height [ form ]


updatedScalingFactor : Signal Float
updatedScalingFactor =
  (Signal.map scalingFactor Window.dimensions)


scaledLogo : Signal Form
scaledLogo =
  Signal.map view model


actionSignal : Signal Action
actionSignal =
  Signal.map UpdateDimensions Window.dimensions


model : Signal Model
model =
  Signal.foldp update init actionSignal


main : Signal Element
main =
  Signal.map2 displayCentered Window.dimensions scaledLogo
