module Main (..) where

import Graphics.Element exposing (Element)
import Window
import Model exposing (Model, init)
import Signal.Extra
import View exposing (view)
import Update exposing (..)


main : Signal Element
main =
  Signal.map view model


actionSignal : Signal Action
actionSignal =
  Signal.map UpdateDimensions Window.dimensions


model : Signal Model
model =
  Signal.Extra.foldp' update (\d -> update d init) actionSignal
