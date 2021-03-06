package core.steps

import models._
import org.joda.time.DateTime

object GateCrossingStep {

  def run(previousState: PlayerState, course: Course, started: Boolean, now: Long)(state: PlayerState): PlayerState = {
    val crossedGates = state.crossedGates
    val step = (previousState.position, state.position)
    val nextGate = course.nextGate(crossedGates.size)
    val newCrossedGates = nextGate match {
      case Some(StartLine) => {
        if (started && course.downwind.crossedUpward(step)) now +: crossedGates
        else crossedGates
      }
      case Some(UpwindGate) => {
        if (course.upwind.crossedUpward(step)) now +: crossedGates
        else if (course.downwind.crossedUpward(step)) crossedGates.tail
        else crossedGates
      }
      case Some(DownwindGate) => {
        if (course.downwind.crossedDownward(step)) now +: crossedGates
        else if (course.upwind.crossedDownward(step)) crossedGates.tail
        else crossedGates
      }
      case None => crossedGates // already finished race
    }
    state.copy(crossedGates = newCrossedGates, nextGate = course.nextGate(newCrossedGates.size))
  }
}
