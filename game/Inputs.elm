module Inputs where

import Game

import Keyboard
import Mouse
import Drag
import Time
import Char

{-- Part 1: Model the user input ----------------------------------------------

What information do you need to represent all relevant user input?

Task: Redefine `UserInput` to include all of the information you need.
      Redefine `userInput` to be a signal that correctly models the user
      input as described by `UserInput`.

------------------------------------------------------------------------------}

type UserArrows = { x:Int, y:Int }

type KeyboardInput =
  { arrows: UserArrows
  , lockAngle: Bool
  , tack: Bool
  , fineTurn: Bool
  , spellCast: Bool
  , startCountdown: Bool
  , zoom: Int
  }

type MouseInput = { drag: Maybe (Int,Int), mouse: (Int,Int) }

type RaceInput =
  { now: Time
  , startTime: Maybe Time
  , course: Maybe Game.Course
  , crossedGates: [Time]
  , nextGate: Maybe String
  , wind: Game.Wind
  , opponents: [Game.Opponent]
  , buoys: [Game.Buoy]
  , playerSpell: Maybe Game.Spell
  , triggeredSpells: [Game.Spell]
  , leaderboard: [String]
  , isMaster: Bool
  }

mouseInput : Signal MouseInput
mouseInput = lift2 MouseInput (Drag.lastPosition (20 * Time.millisecond)) Mouse.position

zoomInput : Signal Int
zoomInput = lift2 (\plus minus -> if | plus      -> 1
                                     | minus     -> -1
                                     | otherwise -> 0)
                  (Keyboard.isDown (Char.toCode 'Z')) (Keyboard.isDown (Char.toCode 'U'))

keyboardInput : Signal KeyboardInput
keyboardInput = lift7 KeyboardInput
  Keyboard.arrows Keyboard.enter Keyboard.space Keyboard.shift
  (Keyboard.isDown (Char.toCode 'S')) (Keyboard.isDown (Char.toCode 'C'))
  zoomInput


chrono : Signal Time
chrono = foldp (+) 0 (fps 1)

type Input = { delta: Float, chrono: Time, keyboardInput: KeyboardInput, mouseInput: MouseInput,
               windowInput: (Int,Int), raceInput: RaceInput }

