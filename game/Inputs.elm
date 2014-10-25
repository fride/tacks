module Inputs where

import Game

import Keyboard
import Mouse
import Drag
import Time
import Char
import Graphics.Input(Input,input)

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type UserArrows = { x:Int, y:Int }

type KeyboardInput =
  { arrows: UserArrows
  , lock: Bool
  , tack: Bool
  , subtleTurn: Bool
  , startCountdown: Bool
  }

type MouseInput = { drag: Maybe (Int,Int), mouse: (Int,Int) }

type RaceInput =
  { now: Time
  , startTime: Maybe Time
  , course: Maybe Game.Course
  , playerState: Maybe Game.PlayerState
  , wind: Game.Wind
  , opponents: [Game.PlayerState]
  , leaderboard: [Game.PlayerTally]
  , isMaster: Bool
  }

mouseInput : Signal MouseInput
mouseInput = lift2 MouseInput (Drag.lastPosition (20 * Time.millisecond)) Mouse.position

keyboardInput : Signal KeyboardInput
keyboardInput = lift5 KeyboardInput
  Keyboard.arrows
  Keyboard.enter
  Keyboard.space
  Keyboard.shift
  (Keyboard.isDown (Char.toCode 'C'))

chrono : Signal Time
chrono = foldp (+) 0 (fps 1)

type GameInput =
  { delta: Float
  , chrono: Time
  , keyboardInput: KeyboardInput
  , mouseInput: MouseInput
  , windowInput: (Int,Int)
  , raceInput: RaceInput
  }

watchedPlayer : Input (Maybe String)
watchedPlayer = input Nothing

type WatcherOutput = { watchedPlayerId: Maybe String }
