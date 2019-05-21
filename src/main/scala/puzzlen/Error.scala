package puzzlen

sealed trait PuzzleError extends Throwable

object PuzzleError {

  case object Unsolvable extends PuzzleError

  case object ValidationError extends PuzzleError

  final case class ParsingError(msg: String) extends PuzzleError

}
