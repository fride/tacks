module Game where

import Geo (..)
import Core

import Maybe as M
import Time (..)
import List (..)


type alias Gate = { y: Float, width: Float }
type alias Island = { location : Point, radius : Float }
type alias RaceArea = { rightTop: Point, leftBottom: Point }

areaBox : RaceArea -> (Point,Point)
areaBox {rightTop,leftBottom} =
  (rightTop, leftBottom)

areaDims : RaceArea -> (Float,Float)
areaDims {rightTop,leftBottom} =
  let
    (r,t) = rightTop
    (l,b) = leftBottom
  in
    (r - l, t - b)

areaTop : RaceArea -> Float
areaTop {rightTop} = snd rightTop

areaBottom : RaceArea -> Float
areaBottom {leftBottom} = snd leftBottom

areaWidth : RaceArea -> Float
areaWidth = areaDims >> fst

areaHeight : RaceArea -> Float
areaHeight = areaDims >> snd

areaCenters : RaceArea -> (Float,Float)
areaCenters {rightTop,leftBottom} =
  let
    (r,t) = rightTop
    (l,b) = leftBottom
    cx = (r + l) / 2
    cy = (t + b) / 2
  in
    (cx, cy)

genX : Float -> Float -> RaceArea -> Float
genX seed margin area =
  let
    effectiveWidth = (areaWidth area) - margin * 2
    (cx,_) = areaCenters area
  in
    (Core.floatMod seed effectiveWidth) - effectiveWidth / 2 + cx


type alias Vmg =
  { angle: Float
  , speed: Float
  , value: Float
  }

type alias WindGenerator =
  { wavelength1: Float
  , amplitude1: Float
  , wavelength2: Float
  , amplitude2: Float
  }

type alias GustDef =
  { angle: Float
  , speed: Float
  , radius: Float
  }

type alias GustGenerator =
  { interval: Int
  , defs: List GustDef
  }

type alias Course =
  { upwind:           Gate
  , downwind:         Gate
  , laps:             Int
  , markRadius:       Float
  , islands:          List Island
  , area:             RaceArea
  , windGenerator:    WindGenerator
  , gustGenerator:    GustGenerator
  , windShadowLength: Float
  , boatWidth:        Float
  }

type alias Player =
  { id:     String
  , handle: Maybe String
  , status: Maybe String
  , avatarId: Maybe String
  , vmgMagnet: Int
  , guest: Bool
  , user: Bool
  }

type ControlMode = FixedAngle | FixedHeading

type GateLocation = DownwindGate | UpwindGate | StartLine

type alias PlayerState =
  { player:          Player
  , position:        Point
  , isGrounded:      Bool
  , isTurning:       Bool
  , heading:         Float
  , velocity:        Float
  , vmgValue:        Float
  , windAngle:       Float
  , windOrigin:      Float
  , windSpeed:       Float
  , downwindVmg:     Vmg
  , upwindVmg:       Vmg
  , shadowDirection: Float
  , trail:           List Point
  , controlMode:     ControlMode
  , tackTarget:      Maybe Float
  , crossedGates:    List Time
  , nextGate:        Maybe GateLocation
  }

upwind s = abs s.windAngle < 90
closestVmgAngle s = if upwind s then s.upwindVmg.angle else s.downwindVmg.angle
windAngleOnVmg s = if s.windAngle < 0 then -(closestVmgAngle s) else closestVmgAngle s
headingOnVmg s = ensure360 (s.windOrigin + closestVmgAngle s)
deltaToVmg s = s.windAngle - windAngleOnVmg s

asOpponentState : Time -> PlayerState -> OpponentState
asOpponentState now {position,heading,velocity,windAngle,windOrigin,shadowDirection,crossedGates} =
  { time = now
  , position = position
  , heading = heading
  , velocity = velocity
  , windAngle = windAngle
  , windOrigin = windOrigin
  , shadowDirection = shadowDirection
  , crossedGates = crossedGates
  }


type alias OpponentState =
  { time: Float
  , position: Point
  , heading: Float
  , velocity: Float
  , windAngle: Float
  , windOrigin: Float
  , shadowDirection: Float
  , crossedGates: List Time
  }

type alias Opponent =
  { player: Player
  , state: OpponentState
  }

type alias PlayerTally = { playerId: String, playerHandle: Maybe String, gates: List Time }

type alias GhostState =
  { position: Point
  , heading:  Float
  , id:       String
  , handle:   Maybe String
  , gates:    List Time
  }

type alias Gust =
  { position : Point
  , angle: Float
  , speed: Float
  , radius: Float
  , maxRadius: Float
  , spawnedAt: Float
  }

type alias Wind =
  { origin : Float
  , speed : Float
  , gusts : List Gust
  , gustCounter : Int
  }

type GameMode = Race | TimeTrial

type alias RaceChat =
  { active: Bool
  , buffer: String
  , messages: List (String, String)
  }

type alias Timing =
  { now:         Time
  , serverNow:   Time
  , countdown:   Float
  , startTime:   Maybe Time
  , creationTime: Time
  , localTime:   Time
  , roundTripDelay: Float
  }

type alias GameState =
  { wind:        Wind
  , playerState: PlayerState
  , wake:        List Point
  , center:      Point
  , opponents:   List Opponent
  , ghosts:      List GhostState
  , course:      Course
  , leaderboard: List PlayerTally
  , isMaster:    Bool
  , gameMode:    GameMode
  , live:        Bool
  , timing:      Timing
  , chat:        RaceChat
  }

serverClock : GameState -> Float
serverClock {timing,gameMode} =
  case gameMode of
    Race ->
      timing.now - timing.creationTime
    TimeTrial ->
      case timing.startTime of
        Just t ->
          timing.now - t + timing.countdown * 1000
        Nothing ->
          0


type alias GameSetup =
  { now: Time
  , creationTime: Time
  , countdown: Float
  , course: Course
  , player: Player
  , timeTrial: Bool
  }

defaultVmg : Vmg
defaultVmg = { angle = 0, speed = 0, value = 0}

defaultPlayer : Player
defaultPlayer =
  { id = ""
  , handle = Nothing
  , user = False
  , guest = False
  , status = Nothing
  , avatarId = Nothing
  , vmgMagnet = 0
  }

defaultPlayerState : Player -> PlayerState
defaultPlayerState player =
  { player          = player
  -- , time            = now
  , position        = (0,0)
  , isGrounded      = False
  , isTurning       = False
  , heading         = 0
  , velocity        = 0
  , vmgValue        = 0
  , windAngle       = 0
  , windOrigin      = 0
  , windSpeed       = 0
  , downwindVmg     = defaultVmg
  , upwindVmg       = defaultVmg
  , shadowDirection = 0
  , trail           = []
  , controlMode     = FixedHeading
  , tackTarget      = Nothing
  , crossedGates    = []
  , nextGate        = Just StartLine
  }


defaultGate : Gate
defaultGate = { y = 0, width = 0 }

--defaultCourse : Course
--defaultCourse =
--  { upwind           = defaultGate
--  , downwind         = defaultGate
--  , laps             = 0
--  , markRadius       = 0
--  , islands          = []
--  , area             = { rightTop = (0,0), leftBottom = (0,0) }
--  , windShadowLength = 0
--  , boatWidth        = 0
--  }

defaultWind : Float -> Wind
defaultWind now =
  { origin = 0
  , speed  = 0
  , gusts  = []
  , gustCounter = 0
  }

defaultTiming : Float -> Float -> Float -> Timing
defaultTiming now countdown creationTime =
  { now         = now
  , serverNow   = now
  , countdown   = countdown
  , startTime   = Nothing
  , creationTime = creationTime
  , localTime   = 0
  , roundTripDelay = 0
  }

defaultChat : RaceChat
defaultChat =
  { active = False
  , buffer = ""
  , messages = []
  }

defaultGame : GameSetup -> GameState
defaultGame {now,creationTime,course,player,timeTrial,countdown} =
  { wind        = defaultWind now
  , playerState = defaultPlayerState player
  , center      = (0,0)
  , wake        = []
  , opponents   = []
  , ghosts      = []
  , course      = course
  , leaderboard = []
  , isMaster    = False
  , gameMode    = if timeTrial then TimeTrial else Race
  , live        = False
  , timing      = defaultTiming now countdown creationTime
  , chat        = defaultChat
  }

getGateMarks : Gate -> (Point,Point)
getGateMarks gate = ((-gate.width / 2, gate.y), (gate.width / 2, gate.y))

findPlayerGhost : String -> List GhostState -> Maybe GhostState
findPlayerGhost playerId ghosts =
  Core.find (\g -> g.id == playerId) ghosts

findOpponent : List PlayerState -> String -> Maybe PlayerState
findOpponent opponents id =
  let filtered = filter (\ps -> ps.player.id == id) opponents
  in  if isEmpty filtered then Nothing else Just (head filtered)

raceTime : GameState -> Float
raceTime {timing} =
  case timing.startTime of
    Just t -> timing.now - t
    Nothing -> 0

isStarted : GameState -> Bool
isStarted {timing} =
  case timing.startTime of
    Just t -> timing.now >= t
    Nothing -> False
