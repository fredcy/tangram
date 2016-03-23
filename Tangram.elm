module Main (..) where

import Graphics.Element exposing (Element)
import Window
import Model exposing (Model, init)
import Signal.Extra
import View exposing (view)
import Update exposing (..)
import Task
import Effects
import Start exposing (start, Config, App)


main : Signal Element
main =
  app.output


dimensionSignal : Signal Action
dimensionSignal =
  Signal.map UpdateDimensions Window.dimensions


app =
  start
    { init = (Model.init, Effects.none)
    , update = update
    , view = view
    , inputs = [ dimensionSignal ]
    }

