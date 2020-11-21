package http

import cats.effect.{Blocker, ExitCode, IO, IOApp}
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.client.blaze.BlazeClientBuilder

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext

object GuessClient extends IOApp {
  private val uri = uri"http://localhost:9003"

  def guess(client: Client[IO], gameId: String,  min: Int, max: Int): IO[Option[Unit]] = {
    val middle = (max + min) / 2
    client.expect[String](uri / "guess" / gameId / middle.toString).flatMap {
      case "Too big" => guess(client, gameId, min, middle )
      case "Too small" => guess(client, gameId, middle, max)
      case "Out of attempts" => IO(None)
      case _ => IO(Some(()))
    }
  }


  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource
      .parZip(Blocker[IO]).use { case (client, blocker) =>
      for {
        gameId <- client.expect[String](uri / "start")
        _ <- IO(println(gameId))
        out <- guess(client, gameId, 0, 10)
      } yield ()

    }.as(ExitCode.Success)
}
