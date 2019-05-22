package puzzlen

import atto.Atto._
import atto.Parser
import cats.MonadError
import puzzlen.PuzzleError.ValidationError
import puzzlen.Tile.Empty

trait BoardValidator[F[_]] {
  def validate(board: Board): F[Unit]
}

object BoardValidator {

  def apply[F[_]](implicit F: MonadError[F, Throwable], parser: Parser[Tile] = BoardParser.tile): BoardValidator[F] = (board: Board) => {
    val tiles: Set[Tile] = (0 until board.n * board.n).toList.flatMap { case x =>
      parser.parseOnly(x.toString).option
    }.toSet
    if (board.tilesSet.size == tiles.size && (board.tilesSet diff tiles).isEmpty) F.unit else F.raiseError(ValidationError)
  }


}


