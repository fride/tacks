module Render.Race where

import Render.Utils (..)
import Core (..)
import Geo (..)
import Game (..)
import String
import Text
import Graphics.Collage

renderStartLine : Gate -> Float -> Bool -> Time -> Float -> Form
renderStartLine gate markRadius started timer sc =
  let lineStyle = if started then dotted green else solid orange
      line = segment left right |> traced lineStyle |> alpha a
      (left,right) = getGateMarks gate
      a = if started then 0.5 + 0.5 * cos (timer * 0.005) else 1
      marks = map (\g -> circle markRadius |> filled colors.gateMark |> move g) [left, right]
  in  group (line :: marks) |> scale sc

renderGate : Gate -> Float -> Bool -> Float -> Form
renderGate gate markRadius isNext sc =
  let (left,right) = getGateMarks gate
      lineStyle = if isNext
        then traced (dotted colors.gateLine)
        else traced (solid colors.seaBlue)
      line = segment left right |> lineStyle
      leftMark = circle markRadius |> filled colors.gateMark |> move left
      rightMark = circle markRadius |> filled colors.gateMark |> move right
  in  group [line, leftMark, rightMark] |> scale sc

renderPlayerAngles : Float -> Player -> Form
renderPlayerAngles sc player =
  let windOriginRadians = toRadians (player.direction - player.windAngle)
      windMarker = polygon [(0,4),(-3,-4),(3,-4)]
        |> filled white
        |> rotate (windOriginRadians + pi/2)
        |> scale sc
        |> move (scalePoint sc (fromPolar (20, windOriginRadians)))
        |> alpha 0.5
      windAngleText = (show (abs (round player.windAngle))) ++ "&deg;"
        |> scaledText (sc * 0.8)
        |> (if player.controlMode == FixedWindAngle then line Under else id)
        |> centered |> toForm
        |> move (scalePoint sc (fromPolar (20, windOriginRadians + pi)))
        |> alpha 0.5
  in  group [windMarker, windAngleText]

renderEqualityLine : Point -> Float -> Float -> Form
renderEqualityLine (x,y) windOrigin sc =
  let left = (fromPolar (50 * sc, toRadians (windOrigin - 90)))
      right = (fromPolar (50 * sc, toRadians (windOrigin + 90)))
  in  segment left right |> traced (dotted white) |> alpha 0.2

renderWake : [Point] -> Float -> Form
renderWake wake sc =
  let scaledPoints = map (scalePoint sc) wake
      pairs = if (isEmpty scaledPoints) then [] else zip scaledPoints (tail scaledPoints) |> indexedMap (,)
      style = { defaultLine | color <- white, width <- 3 }
      opacityForIndex i = 0.3 - 0.3 * (toFloat i) / (toFloat (length scaledPoints))
      renderSegment (i,(a,b)) = segment a b |> traced style |> alpha (opacityForIndex i)
  in  group (map renderSegment pairs)

renderBoat : String -> Float -> Form
renderBoat filename sc =
  let picHeight = 20
      picWidth = 11
      realLength = 13.45 -- length overall
      w = realLength * picWidth / picHeight
      scaledLength = realLength * sc
      scaledWidth = scaledLength * picWidth / picHeight
  in  image (round scaledWidth) (round scaledLength) ("/assets/images/" ++ filename ++ ".png") |> toForm

renderFog : Float -> Player -> [Spell] -> [Form]
renderFog sc player spells =
  let fog1 = oval 190 250
        |> filled grey
        |> rotate (snd player.position / 60)
      fog2 = oval 170 230
        |> filled white
        |> rotate (fst player.position / 41 + 220)
        |> alpha 0.8
  in  if (containsSpell "Fog" spells) then [fog1, fog2] |> map (scale sc) else []

renderPlayer : Player -> [Spell] -> Float -> Form
renderPlayer player spells sc =
  let boatPath = if(containsSpell "PoleInversion" spells) then "boat-pole-inversion" else "icon-ac72"
      hull = renderBoat boatPath sc
        |> rotate (toRadians (player.direction + 90))
        --|> scale sc
      helpers = if sc > 1
        then [renderPlayerAngles sc player, renderEqualityLine player.position player.windOrigin sc]
        else []
      fog = renderFog sc player spells
      movingPart = group (helpers ++ [hull] ++ fog) |> move (scalePoint sc player.position)
      wake = renderWake player.wake sc
  in group [movingPart, wake]

renderOpponent : Float -> Opponent -> Form
renderOpponent sc opponent =
  let p = (scalePoint sc opponent.position)
      hull = image 11 20 "/assets/images/icon-ac72.png" |> toForm
        |> rotate (toRadians (opponent.direction + 90))
        |> move p
        |> alpha 0.5
      name = opponent.name |> baseText |> centered |> toForm
        |> move (add p (0,-25))
        |> alpha 0.3
  in group [hull, name]


renderBounds : (Point, Point) -> Float -> Form
renderBounds box sc =
  let (ne,sw) = box
      w = fst ne - fst sw
      h = snd ne - snd sw
      cw = (fst ne + fst sw) / 2
      ch = (snd ne + snd sw) / 2
  in rect w h |> outlined (dashed black) --filled colors.seaBlue
              |> alpha 0.3
              |> move (scalePoint sc (cw, ch))
              |> scale sc

renderGust : Wind -> Float -> Gust -> Form
renderGust wind sc gust =
  let a = 0.3 * (abs gust.speed) / 10
      color = if gust.speed > 0 then black else white
  in  circle (gust.radius * sc) |> filled color |> alpha a |> move (scalePoint sc gust.position)

renderGusts : Wind -> Float -> Form
renderGusts wind sc =
  map (renderGust wind sc) wind.gusts |> group

renderIsland : Island -> Form
renderIsland {location,radius} =
  circle radius |> filled colors.sand |> move location

renderIslands : Course -> Float -> Form
renderIslands course sc =
  group (map renderIsland course.islands) |> scale sc

renderBuoy : Time -> Float -> Buoy -> Form
renderBuoy timer sc {position,radius,spell} =
  let a = 0.4 + 0.2 * cos (timer * 0.005)
  in  circle radius |> filled colors.buoy |> move (scalePoint sc position) |> alpha a |> scale sc

renderGateLaylines : Float -> Float -> Gate -> Float -> Form
renderGateLaylines vmg windOrigin gate sc =
  let vmgRad = toRadians vmg
      (leftMark,rightMark) = getGateMarks gate
      windAngleRad = toRadians windOrigin
      leftLineEnd = add leftMark (fromPolar (1000, windAngleRad + vmgRad + pi/2))
      rightLineEnd = add rightMark (fromPolar (1000, windAngleRad - vmgRad - pi/2))
      drawLine (p1,p2) = segment p1 p2 |> traced (solid white)
  in  group (map drawLine [(leftMark, leftLineEnd), (rightMark, rightLineEnd)]) |> alpha 0.3 |> scale sc

renderLaylines : Player -> Course -> Float -> Maybe Form
renderLaylines player course sc =
  case player.nextGate of
    Just Upwind   -> Just <| renderGateLaylines player.upwindVmg player.windOrigin course.upwind sc
    Just Downwind -> Just <| renderGateLaylines player.downwindVmg player.windOrigin course.downwind sc
    _             -> Nothing

formatCountdown : Time -> String
formatCountdown c =
  let cs = c |> inSeconds |> ceiling
      m = cs `div` 60
      s = cs `rem` 60
  in  "Start in " ++ (show m) ++ "'" ++ (show s) ++ "\"..."

renderCountdown : GameState -> Maybe Form
renderCountdown gameState =
  let messageBuilder msg = baseText msg |> centered |> toForm
        |> move (0, gameState.course.downwind.y * gameState.scale + 40)
  in  case gameState.countdown of
        Just c ->
          if c > 0
            then Just <| messageBuilder (formatCountdown (getCountdown gameState.countdown))
            else if gameState.player.nextGate == Just StartLine
              then Just <| messageBuilder "Go!"
              else Nothing
        Nothing ->
          if gameState.isMaster
            then Just <| messageBuilder startCountdownMessage
            else Nothing

renderFinished : Course -> Player -> Float -> Maybe Form
renderFinished course player sc =
  case player.nextGate of
    Nothing -> Just (baseText "Finished!" |> centered |> toForm |> move (0, course.downwind.y * sc + 40))
    _       -> Nothing

renderRace : GameState -> Form
renderRace ({player,opponents,course,buoys,triggeredSpells,now} as gameState) =
  let sc = gameState.scale
      downwindOrStartLine = if isEmpty player.crossedGates
        then renderStartLine course.downwind course.markRadius (isStarted gameState.countdown) now sc
        else renderGate course.downwind course.markRadius (player.nextGate == Just Downwind) sc
      justForms =
        [ renderBounds course.bounds sc
        , renderIslands course sc
        , downwindOrStartLine
        , renderGate course.upwind course.markRadius (player.nextGate == Just Upwind) sc
        , group (map (renderOpponent sc) opponents)
        , renderGusts gameState.wind sc
        , renderPlayer player triggeredSpells sc
        ]
      maybeForms =
        [ renderCountdown gameState
        , mapMaybe (\c -> group (map (renderBuoy c sc) buoys)) gameState.countdown
        , renderLaylines player course sc
        , renderFinished gameState.course player sc
        ]
  in  (group (justForms ++ (compact maybeForms))) |> move (neg (scalePoint sc player.center))

