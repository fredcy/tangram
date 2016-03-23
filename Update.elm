module Update (Action(..), update) where

import Effects exposing (Effects)
import Model exposing (..)
import Time exposing (Time)


type Action
  = NoOp
  | UpdateDimensions ( Int, Int )
  | Click
  | Tick Time


update : Action -> Model -> ( Model, Effects Action )
update action model =
  case action of
    UpdateDimensions dimensions ->
      ( { model | dimensions = dimensions }, Effects.none )

    Click ->
      case model.animation of
        AnimationIdle ->
          ( { model | animation = AnimationStarting 1000 }
          , Effects.tick Tick
          )

        _ ->
          ( model, Effects.none )

    Tick time ->
      case model.animation of
        AnimationIdle ->
          ( model, Effects.none ) |> Debug.log "error: got Tick while idle"

        AnimationStarting duration ->
          let
            animation =
              AnimationActive
                { duration = duration
                , startTime = time
                , startPieces = model.pieces
                }
          in
            ( { model | animation = animation }, Effects.tick Tick )

        AnimationActive { duration, startTime, startPieces } ->
          let
            fraction =
              (time - startTime) / duration
          in
            if fraction >= 1.0 then
              ( { model | animation = AnimationIdle } |> makePerson 1 startPieces
              , Effects.none
              )
            else
              ( model |> makePerson fraction startPieces
              , Effects.tick Tick
              )

    NoOp ->
      ( model, Effects.none )


animate : Float -> Tangram -> Model -> Model
animate fraction startPieces model =
  let
    travel =
      0.35

    square =
      startPieces.square

    square' =
      { square | rotation = startPieces.square.rotation + fraction * (degrees 90) }

    medTri =
      startPieces.mediumTriangle

    medTri' =
      { medTri | position = moveOutBack ( travel, travel ) fraction medTri.position }

    smTri =
      startPieces.smallTriangleSE

    smTri' =
      { smTri | position = moveOutBack ( travel, -travel ) fraction smTri.position }

    bigTri =
      startPieces.bigTriangleS

    bigTri' =
      { bigTri | position = moveOutBack ( -travel, -travel ) fraction bigTri.position }

    par =
      startPieces.parallelogram

    par' =
      { par | position = moveOutBack ( -travel, travel ) fraction par.position }

    smTri2 =
      startPieces.smallTriangleN

    smTri2' =
      { smTri2 | position = moveOutBack ( -travel, travel ) fraction smTri2.position }

    pieces =
      model.pieces

    pieces' =
      { pieces
        | square = square'
        , mediumTriangle = medTri'
        , smallTriangleSE = smTri'
        , bigTriangleS = bigTri'
        , parallelogram = par'
        , smallTriangleN = smTri2'
      }
  in
    { model | pieces = pieces' }


moveOutBack : ( Float, Float ) -> Float -> ( Float, Float ) -> ( Float, Float )
moveOutBack ( dx, dy ) fraction ( px, py ) =
  let
    fraction' =
      if fraction < 0.5 then
        fraction
      else
        1 - fraction
  in
    ( px + fraction' * dx, py + fraction' * dy )


moveBy : Position -> Float -> Position -> Position
moveBy ( dx, dy ) fraction ( px, py ) =
  ( px + fraction * dx, py + fraction * dy )


makePerson : Float -> Tangram -> Model -> Model
makePerson fraction tangram model =
  let
    bigTriangleS' =
      modify tangram.bigTriangleS (0, 0.25) 0 fraction

    bigTriangleW' =
      modify tangram.bigTriangleW (1, 0.25) -90 fraction

    mediumTriangle' =
      modify tangram.mediumTriangle (0, -1.75) 0 fraction

    smTri1 =
      modify tangram.smallTriangleSE ( 0, -1.25 ) 45 fraction

    smTri2 =
      modify tangram.smallTriangleN (-1.75, -1.5) -135 fraction

    square' =
      modify tangram.square ( 0, 1.25 ) 0 fraction

    parallelogram' =
      modify tangram.parallelogram (-0.7, -1.85) 45 fraction

    pieces =
      { tangram
        | smallTriangleSE = smTri1
        , smallTriangleN = smTri2
        , square = square'
        , mediumTriangle = mediumTriangle'
        , parallelogram = parallelogram'
        , bigTriangleS = bigTriangleS'
        , bigTriangleW = bigTriangleW'
      }
  in
    { model | pieces = pieces }


modify : Piece -> Position -> Rotation -> Float -> Piece
modify basePiece ( dx, dy ) dRotation fraction =
  let
    ( px, py ) =
      basePiece.position

    position' =
      ( px + fraction * dx, py + fraction * dy )

    rotation' =
      basePiece.rotation + fraction * (degrees dRotation)
  in
    { basePiece | position = position', rotation = rotation' }
