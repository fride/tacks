module Main where

import Window
import Signal (..)
import Time (..)
import Graphics.Element (Element)

import Inputs (..)
import Game
import Steps
import Render.All as R
import Render.Utils

port raceInput : Signal RaceInput

port gameSetup : Game.GameSetup

clock : Signal Clock
clock = map (\(time,delta) -> { time = time, delta = delta }) (timestamp (fps 30))

input : Signal GameInput
input = sampleOn clock <| map5 GameInput
  clock
  --chrono
  keyboardInput
  chatInput
  Window.dimensions
  raceInput

initialState : Game.GameState
initialState = Game.defaultGame gameSetup

gameState : Signal Game.GameState
gameState = foldp Steps.stepGame initialState input

port playerOutput : Signal PlayerOutput
port playerOutput = map3 PlayerOutput
  ((\gs -> Game.asOpponentState gs.timing.now gs.playerState) <~ gameState)
  (.keyboardInput <~ input)
  (.timing >> .localTime <~ gameState)

port title : Signal String
port title = Render.Utils.gameTitle <~ gameState

main : Signal Element
main = map2 R.render Window.dimensions gameState
