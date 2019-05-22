package puzzlen


import atto.Atto._
import atto.ParseResult.{Done, Fail}
import atto._
import cats.implicits._
import cats.{Functor, MonadError, Traverse}
import puzzlen.PuzzleError.ParsingError
import puzzlen.Tile.{Empty, Number}

trait BoardParser[F[_]] {
  def parse(raw: String): F[Board]
}

object BoardParser {
  val min = 2
  val max = 5

  implicit val dim: Parser[Int] = int.filter(d => d >= min && d <= max)

  implicit val tile: Parser[Tile] = {
    for {
      v <- int
    } yield if (v == 0) Empty else Number(v)
  }

  def tiles(n: Int): Parser[List[Tile]] = manyN(n, tile <~ char(' '))

  implicit val board: Parser[Board] =
    for {
      d <- dim <~ char(' ')
      ts <- tiles(d * d - 1)
      last <- tile
    } yield Board(d, ts ::: last :: Nil)


  def apply[F[_]](implicit F: MonadError[F, Throwable], parser: Parser[Board] = board): BoardParser[F] = (raw: String) => parser.parseOnly(raw) match {
    case Done(_, b) =>
      F.pure(b)
    case Fail(_, _, message) =>
      F.raiseError(ParsingError(message))
  }
}


