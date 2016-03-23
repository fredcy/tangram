module Update (Action(..), update) where

import Model exposing (..)

type Action
  = NoOp
  | UpdateDimensions ( Int, Int )


update : Action -> Model -> Model
update action model =
  case action of
    UpdateDimensions dimensions ->
      { model | dimensions = dimensions }

    NoOp ->
      model

