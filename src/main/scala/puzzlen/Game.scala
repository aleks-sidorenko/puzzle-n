package puzzlen

import cats.MonadError
import puzzlen.Tile.Empty


sealed trait Move {
  def back: Move = {
    this match {
      case Move.Up => Move.Down
      case Move.Down => Move.Up
      case Move.Left => Move.Right
      case Move.Right => Move.Left
    }
  }
}

object Move {
  val all: Set[Move] = Set(Up, Down, Left, Right)

  case object Up extends Move

  case object Down extends Move

  case object Right extends Move

  case object Left extends Move

}


sealed trait Tile

object Tile {

  case object Empty extends Tile

  case class Number(value: Int) extends Tile

}

case class Board(n: Int, emptyPosition: Board.Position, board: Map[Board.Position, Tile]) {
  def solved: Boolean = ???

  val validMoves: List[Move] = scala.util.Random.shuffle(Move.all.filter(m => pos(m).isDefined).toList)

  private def pos(m: Move): Option[Board.Position] = {
    val (x, y) = emptyPosition
    m match {
      case Move.Up if x != 0 => Some(x - 1 -> y)
      case Move.Down if x != n - 1 => Some(x + 1 -> y)
      case Move.Left if y != 0 => Some(x, y - 1)
      case Move.Right if y != n - 1 => Some(x, y + 1)
      case _ => None
    }
  }

  def move(m: Move): Option[Board] = {
    pos(m).map { newPos =>
      val tile = board(newPos)
      copy(
        emptyPosition = newPos,
        board = board + (newPos -> Empty) + (emptyPosition -> tile))
    }
  }

  def tick: List[Board] = validMoves.flatMap(move(_))
}

object Board {
  type Position = (Int, Int)

  def apply(n: Int, tiles: List[Tile]): Board = {
    val board = tiles.zipWithIndex.map { case (t, i) => (i / n, i % n) -> t }
    val emptyPos = board.collect { case (p, Empty) => p }.head
    Board(n, emptyPos, board.toMap)
  }

  def goal(n: Int): Board = {
    val tiles: List[Tile] = (1 until n * n).map(Tile.Number).toList
    apply(n, tiles ++ (Empty :: Nil))
  }
}


trait Solver[F[_]] extends (Board => F[List[Move]])

object Solver {

  def apply[F[_]](implicit F: MonadError[F, Throwable]): Solver[F] = (board: Board) => {
    F.pure(List.empty)
  }
}

