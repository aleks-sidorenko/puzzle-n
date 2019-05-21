package puzzlen

import cats.{Monad, MonadError}
import puzzlen.PuzzleError.ParsingError

import scala.util.Try

trait Terminal[F[_]] {
  def putLn(value: String): F[Unit]

  def readLn: F[String]

  def readInt()(implicit F: MonadError[F, Throwable]): F[Int] = F.flatMap(readLn) { l =>
    Try(l.toInt).map(F.pure).getOrElse(F.raiseError(ParsingError(s"Cannot parse '$l' to integer")))
  }
}

object Terminal {
  def apply[F[_]](implicit F: Monad[F]): Terminal[F] = new Terminal[F] {
    override def putLn(value: String): F[Unit] = F.pure(println(value))

    override def readLn: F[String] = F.pure(scala.io.StdIn.readLine)
  }
}
