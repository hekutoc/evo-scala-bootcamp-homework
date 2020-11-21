package http

import java.util.concurrent.atomic.AtomicReference

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder

import scala.concurrent.ExecutionContext
import scala.util.Random

object GuessServer extends IOApp {

  import scala.collection.mutable.Map

  object MinMatcher      extends QueryParamDecoderMatcher[Int]("min")
  object MaxMatcher      extends QueryParamDecoderMatcher[Int]("max")
  object AttemptsMatcher extends QueryParamDecoderMatcher[Int]("attempts")


  val store: Map[String, AtomicReference[GuessGameState]] = Map()
  private val routes = HttpRoutes.of[IO] {

    // curl "localhost:9003/start?min=0&max=10&attempts=10"
    case GET -> Root / "start"
      :? MinMatcher(min)
      +& MaxMatcher(max)
      +& AttemptsMatcher(attempts) =>
      val id = randomString(10)
      store.addOne((id, new AtomicReference(GuessGameState.init(min, max, attempts))))
      Ok(id)

    // curl localhost:9003/guess/kX0Gc4NfXY/3
    case GET -> Root / "guess" / gameId / IntVar(value) =>
       store.get(gameId) match {
         case None => NotFound("Game with such ID was not found")
         case Some(game) =>
           val state = game.getAndUpdate(prev => GuessGameState.update(prev))
           GuessGameState.guess(state, value) match {
             case None => Ok(s"Correct! It's ${value}")
             case Some(GuessFailure.TooBig) => Ok(s"Too big")
             case Some(GuessFailure.TooSmall) => Ok(s"Too small")
             case Some(GuessFailure.OutOfAttempts) => Ok(s"Out of attempts")
           }
       }
  }

  private[http] val httpApp = {
    routes
  }.orNotFound


  def randomString(length: Int) = Random.alphanumeric.take(length).mkString("")

  override def run(args: List[String]): IO[ExitCode] = BlazeServerBuilder[IO](ExecutionContext.global)
    .bindHttp(port = 9003, host = "localhost")
    .withHttpApp(httpApp)
    .serve
    .compile
    .drain
    .as(ExitCode.Success)
}

