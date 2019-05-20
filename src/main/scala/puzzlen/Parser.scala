package puzzlen

import cats.MonadError

trait Parser[F[_]] {
  def parse(rows: List[String]): F[Board]
}

object Parser {
  def apply[F[_]](implicit F: MonadError[F, Throwable]): Parser[F] = (rows: List[String]) => ???
}


