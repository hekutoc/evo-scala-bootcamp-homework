package http

import scala.util.Random


trait GuessFailure

object GuessFailure {
  final case object OutOfAttempts extends GuessFailure
  final case object TooSmall extends GuessFailure
  final case object TooBig extends GuessFailure
}


case class GuessGameState(number: Int, attemptsLeft: Int)

object GuessGameState {
  def init(min: Int, max: Int, guessCount: Int): GuessGameState = GuessGameState(Random.between(min, max), guessCount)

  def update(state: GuessGameState) = {
    this.checkAttempts(state) match {
      case None => state.copy(attemptsLeft = state.attemptsLeft - 1)
      case Some(_) => state
    }
  }

  def guess(state: GuessGameState, number: Int): Option[GuessFailure] = {
    (this.checkNumber(state, number), this.checkAttempts(state)) match {
      case (_, Some(err)) => Some(err)
      case (Some(err), _) => Some(err)
      case _ => None
    }
  }

  private def checkAttempts(state: GuessGameState): Option[GuessFailure] = {
    if (state.attemptsLeft == 0) Some(GuessFailure.OutOfAttempts)
    else None
  }

  private def checkNumber(state: GuessGameState, number: Int): Option[GuessFailure] = {
    if (number > state.number) {
      Some(GuessFailure.TooBig)
    } else if (number < state.number) {
      Some(GuessFailure.TooSmall)
    } else {
      None
    }
  }
}
