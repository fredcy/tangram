module Main (..) where

import Graphics.Collage exposing (..)
import Graphics.Element exposing (Element)
import Window
import Model exposing (Model, init)
import Signal.Extra exposing (foldp')
import View exposing (..)
import Update exposing (..)


main : Signal Element
main =
  Signal.map2 displayCentered Window.dimensions scaledLogo


scaledLogo : Signal Form
scaledLogo =
  Signal.map view model


actionSignal : Signal Action
actionSignal =
  Signal.map UpdateDimensions Window.dimensions


model : Signal Model
model =
  Signal.Extra.foldp' update (\d -> update d init) actionSignal
