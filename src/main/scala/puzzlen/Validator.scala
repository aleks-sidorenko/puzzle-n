package puzzlen

import atto.Parser
import cats.MonadError
import puzzlen.PuzzleError.ValidationError

trait BoardValidator[F[_]] {
  def validate(board: Board): F[Unit]
}

object BoardValidator {

  def apply[F[_]](implicit F: MonadError[F, Throwable], parser: Parser[Tile] = BoardParser.tile): BoardValidator[F] = (board: Board) => {
    val expectedTiles: Set[Tile] = Board.goal(board.n).tilesSet
    if (board.tilesSet == expectedTiles) F.unit else F.raiseError(ValidationError)
  }


}


