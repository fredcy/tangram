module Update (Action(..), update) where

import Easing
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
                , startTangram = model.tangram
                }
          in
            ( { model | animation = animation }, Effects.tick Tick )

        AnimationActive { duration, startTime, startTangram } ->
          let
            fraction =
              (time - startTime) / duration
            whichAnimation =
              makePerson
          in
            if fraction >= 1.0 then
              ( { model | animation = AnimationIdle } |> whichAnimation 1 startTangram
              , Effects.none
              )
            else
              ( model |> whichAnimation fraction startTangram
              , Effects.tick Tick
              )

    NoOp ->
      ( model, Effects.none )


animate : Float -> Tangram -> Model -> Model
animate fraction startTangram model =
  let
    travel =
      0.35

    square =
      startTangram.square

    square' =
      { square | rotation = startTangram.square.rotation + fraction * (degrees 90) }

    mediumTriangle' =
      movePieceOutBack ( travel, travel ) startTangram.mediumTriangle fraction

    smallTriangle1' =
      movePieceOutBack ( travel, -travel ) startTangram.smallTriangle1 fraction

    bigTriangle1' =
      movePieceOutBack ( -travel, -travel ) startTangram.bigTriangle1 fraction

    parallelogram' =
      movePieceOutBack ( -travel, travel ) startTangram.parallelogram fraction

    smallTriangle2' =
      movePieceOutBack ( -travel, travel ) startTangram.smallTriangle2 fraction

    tangram =
      model.tangram

    tangram' =
      { tangram
        | square = square'
        , mediumTriangle = mediumTriangle'
        , smallTriangle1 = smallTriangle1'
        , bigTriangle1 = bigTriangle1'
        , parallelogram = parallelogram'
        , smallTriangle2 = smallTriangle2'
      }
  in
    { model | tangram = tangram' }


movePieceOutBack : ( Float, Float ) -> Piece -> Float -> Piece
movePieceOutBack (dx, dy) piece fraction =                   
  let
    position' = moveOutBack (dx, dy) fraction piece.position
  in
    { piece | position = position' }


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


makePerson : Float -> Tangram -> Model -> Model
makePerson fraction tangram model =
  let
    bigTriangle1' =
      modify tangram.bigTriangle1 ( 0, 0.25 ) 0 fraction

    bigTriangle2' =
      modify tangram.bigTriangle2 ( 1, 0.25 ) -90 fraction

    mediumTriangle' =
      modify tangram.mediumTriangle ( 0, -1.75 ) 0 fraction

    smTri1 =
      modify tangram.smallTriangle1 ( 0, -1.25 ) 45 fraction

    smTri2 =
      modify tangram.smallTriangle2 ( -1.75, -1.5 ) -135 fraction

    square' =
      modify tangram.square ( 0, 1.25 ) 360 fraction

    parallelogram' =
      modify tangram.parallelogram ( -0.7, -1.85 ) 45 fraction

    tangram' =
      { tangram
        | smallTriangle1 = smTri1
        , smallTriangle2 = smTri2
        , square = square'
        , mediumTriangle = mediumTriangle'
        , parallelogram = parallelogram'
        , bigTriangle1 = bigTriangle1'
        , bigTriangle2 = bigTriangle2'
      }
  in
    { model | tangram = tangram' }


modify : Piece -> Position -> Rotation -> Float -> Piece
modify basePiece ( dx, dy ) dRotation fraction =
  let
    ( px, py ) =
      basePiece.position

    ease from to fraction =
      Easing.ease Easing.easeInExpo Easing.float from to 1 fraction

    position' =
      ( ease px (px + dx) fraction, ease py (py + dy) fraction )

    rotation' =
      ease basePiece.rotation (basePiece.rotation + (degrees dRotation)) fraction
  in
    { basePiece | position = position', rotation = rotation' }
