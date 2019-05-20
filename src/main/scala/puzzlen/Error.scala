package puzzlen

sealed trait PuzzleError extends Throwable

object PuzzleError {

  case object Unsolvable extends PuzzleError

  final case class InvalidInput(msg: String) extends PuzzleError

}
