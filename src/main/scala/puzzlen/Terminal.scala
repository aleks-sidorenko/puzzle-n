package puzzlen

import cats.Monad

trait Terminal[F[_]] {
  def putLn(value: String): F[Unit]

  def readLn: F[String]
}

object Terminal {
  def apply[F[_]](implicit F: Monad[F]): Terminal[F] = new Terminal[F] {
    override def putLn(value: String): F[Unit] = F.pure(println(value))

    override def readLn: F[String] = F.pure(scala.io.StdIn.readLine)
  }
}
