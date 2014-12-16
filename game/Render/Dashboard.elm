module Render.Dashboard where

import Render.Utils (..)
import Core (..)
import Geo (..)
import Game (..)
import Inputs (watchedPlayer)

import String
import Text
import Maybe (maybe,isNothing,isJust)
import Graphics.Input(customButton)

s = spacer 20 20
xs = spacer 5 5

type BoardLine =
  { id:       String
  , handle:   Maybe String
  , position: Maybe Int
  , delta:    Maybe Time
  , watched:  Bool
  }

buildBoardLine : Bool -> BoardLine -> Element
buildBoardLine watching {id,handle,position,delta,watched} =
  let
    watchingText = if watching then (if watched then "* " else "  ") else ""
    positionText = maybe "  " (\p -> show p ++ ".") position
    handleText   = maybe "Anonymous" identity handle |> fixedLength 12
    deltaText    = maybe "-" (\d -> "+" ++ show (d / 1000)) delta
    el = watchingText ++ join " " [positionText, handleText, deltaText]
      |> baseText
      |> leftAligned
  in
    if watching then
      customButton watchedPlayer.handle (Just id) el el el
    else
      el

getOpponent : Bool -> WatchMode -> PlayerState -> Element
getOpponent watching watchMode {player} =
  let
    watched = case watchMode of
      Watching playerId -> playerId == player.id
      NotWatching       -> False
    line =
      { id       = player.id
      , handle   = player.handle
      , position = Nothing
      , delta    = Nothing
      , watched  = watched
      }
  in
    buildBoardLine watching line

getOpponents : GameState -> Element
getOpponents {opponents,watchMode,playerState} =
  let
    watching = isNothing playerState
    allOpponents = maybe opponents (\ps -> ps :: opponents) playerState
  in
    map (getOpponent watching watchMode) allOpponents |> flow down


getLeaderboardLine : Bool -> WatchMode -> PlayerTally -> Int -> PlayerTally -> Element
getLeaderboardLine watching watchMode leaderTally position tally =
  let
    watched = case watchMode of
      Watching playerId -> playerId == tally.playerId
      NotWatching       -> False
    delta = if length tally.gates == length leaderTally.gates
      then Just (head tally.gates - head leaderTally.gates)
      else Nothing
    line =
      { id       = tally.playerId
      , handle   = tally.playerHandle
      , position = Just (position + 1)
      , delta    = delta
      , watched  = watched
      }
  in
    buildBoardLine watching line

getLeaderboard : GameState -> Element
getLeaderboard {leaderboard,watchMode,playerState} =
  if isEmpty leaderboard then
    empty
  else
    let
      leader = head leaderboard
      watching = isNothing playerState
    in
      indexedMap (getLeaderboardLine watching watchMode leader) leaderboard
        |> flow down

getBoard : GameState -> Element
getBoard gameState =
  if | gameState.gameMode == TimeTrial -> empty
     | isEmpty gameState.leaderboard -> getOpponents gameState
     | otherwise -> getLeaderboard gameState

getMode : GameState -> Element
getMode gameState =
  if isNothing gameState.playerState
    then "SPECTATOR MODE" |> baseText |> leftAligned
    else empty

getHelp : Maybe Float -> Element
getHelp countdownMaybe =
  if maybe True (\c -> c > 0) countdownMaybe then
    helpMessage |> baseText |> centered
  else
    empty


-- Main status (big font size)

getMainStatus : GameState -> Element
getMainStatus ({countdown, gameMode, playerState} as gameState) =
  let
    op = if isInProgress gameState then 0.5 else 1
    s = case gameMode of
      TimeTrial -> getTrialTimer gameState
      Race      -> getRaceTimer gameState
  in
    bigText s |> centered |> opacity op

getTrialTimer : GameState -> String
getTrialTimer {countdown, playerState} =
  case (countdown, playerState) of
    (Just c, Just s) ->
      let
        t = if isNothing s.nextGate then head s.crossedGates else c
      in
        formatTimer t (isNothing s.nextGate)
    _ -> ""

getRaceTimer : GameState -> String
getRaceTimer {countdown, playerState} =
  case (countdown, playerState) of
    (Just c, Just s) ->
      let
        t = if isNothing s.nextGate then head s.crossedGates else c
      in
        formatTimer t (isNothing s.nextGate)
    _ -> "start pending"


-- Sub status (normal font size)

getSubStatus : GameState -> Element
getSubStatus ({countdown,isMaster,playerState,course,gameMode} as gameState) =
  let
    op = 1
    s = case countdown of
      Just c ->
        if c > 0
          then "be ready"
          else maybe "" (getFinishingStatus gameState) playerState
      Nothing ->
        if isMaster
          then startCountdownMessage
          else "" -- start pending
  in
    baseText s |> centered |> opacity op


getFinishingStatus : GameState -> PlayerState -> String
getFinishingStatus ({course,gameMode} as gameState) playerState =
  case playerState.nextGate of
    Nothing -> case gameMode of
      Race      -> "finished"
      TimeTrial -> getTimeTrialFinishingStatus gameState playerState
    Just "StartLine" -> "go!"
    _                -> getGatesCount course playerState

getGatesCount : Course -> PlayerState -> String
getGatesCount course player =
  "gate " ++ show (length player.crossedGates) ++ "/" ++ (show (1 + course.laps * 2))

getTimeTrialFinishingStatus : GameState -> PlayerState -> String
getTimeTrialFinishingStatus {playerId,ghosts} {player,crossedGates} =
  case findPlayerGhost playerId ghosts of
    Just playerGhost ->
      let
        previousTime = head playerGhost.gates
        newTime = head crossedGates
      in
        if newTime < previousTime
          then show (newTime - previousTime) ++ "ms\nnew best time!"
          else "+" ++ show (newTime - previousTime) ++ "ms\ntry again?"
    Nothing ->
      case player.handle of
        Just _ -> "you did it!"
        Nothing -> "please create an account to save your run"

--

getWindWheel : Wind -> Element
getWindWheel wind =
  let
    r = 30
    c = circle r |> outlined (solid white)
    windOriginRadians = toRadians wind.origin
    windMarker = polygon [(0,4),(-4,-4),(4,-4)]
      |> filled white
      |> rotate (windOriginRadians + pi/2)
      |> move (fromPolar (r + 4, windOriginRadians))
    windOriginText = ((show (round wind.origin)) ++ "&deg;")
      |> baseText |> centered |> toForm
      |> move (0, r + 25)
    windSpeedText = ((show (round wind.speed)) ++ "kn")
      |> baseText |> centered |> toForm
    legend = "WIND" |> baseText |> centered |> toForm |> move (0, -50)
  in
    collage 80 120 [c, windMarker, windOriginText, windSpeedText, legend]

getVmgBar : PlayerState -> Element
getVmgBar {windAngle,velocity,vmgValue,downwindVmg,upwindVmg} =
  let barHeight = 120
      barWidth = 8
      contour = rect (barWidth + 6) (barHeight + 6)
        |> outlined { defaultLine | width <- 2, color <- white, cap <- Round, join <- Smooth }
        |> alpha 0.5
      theoricVmgValue = if (abs windAngle) < 90 then upwindVmg.value else downwindVmg.value
      boundedVmgValue = if | vmgValue > theoricVmgValue -> theoricVmgValue
                           | vmgValue < 0               -> 0
                           | otherwise                  -> vmgValue
      height = barHeight * boundedVmgValue / theoricVmgValue
      level = rect barWidth height
        |> filled white
        |> move (0, (height - barHeight) / 2)
        |> alpha 0.8
      bar = group [level, contour]
        |> move (0, 10)
      legend = "VMG" |> baseText |> centered |> toForm |> move (0, -(barHeight / 2) - 10)
  in
      collage 80 (barHeight + 40) [bar, legend]


topLeftElements : GameState -> Maybe PlayerState -> [Element]
topLeftElements gameState playerState =
  [ getMode gameState
  , s
  , getBoard gameState
  ]

midTopElement : GameState -> Maybe PlayerState -> Element
midTopElement gameState playerState =
  --getMainStatus gameState `above` getSubStatus gameState
  let
    mainStatus = getMainStatus gameState
    subStatus = getSubStatus gameState
    maxWidth = maximum [widthOf mainStatus, widthOf subStatus]
    mainStatusCentered = container maxWidth (heightOf mainStatus) midTop mainStatus
    subStatusCentered = container maxWidth (heightOf subStatus) midTop subStatus
  in
    mainStatusCentered `above` subStatusCentered

topRightElements : GameState -> Maybe PlayerState -> [Element]
topRightElements {wind,opponents} playerState =
  [ getWindWheel wind
  , s
  , maybe empty getVmgBar playerState
  ]

midBottomElements : GameState -> Maybe PlayerState -> [Element]
midBottomElements {countdown} playerState =
  [ getHelp countdown ]


renderDashboard : GameState -> (Int,Int) -> Element
renderDashboard ({playerId,playerState,opponents,watchMode} as gameState) (w,h) =
  let
    displayedPlayerState = case watchMode of
      Watching playerId -> if selfWatching gameState then findOpponent opponents playerId else Nothing
      NotWatching       -> playerState
  in
    layers
      [ container w h (topLeftAt (Absolute 20) (Absolute 20)) <| flow down (topLeftElements gameState displayedPlayerState)
      , container w h (midTopAt (Relative 0.5) (Absolute 20)) <| midTopElement gameState displayedPlayerState
      , container w h (topRightAt (Absolute 20) (Absolute 20)) <| flow down (topRightElements gameState displayedPlayerState)
      , container w h (midBottomAt (Relative 0.5) (Absolute 20)) <| flow up (midBottomElements gameState displayedPlayerState)
      ]
