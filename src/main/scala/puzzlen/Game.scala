package puzzlen

import cats.MonadError
import puzzlen.PuzzleError.Unsolvable
import puzzlen.Tile.{Empty, Number}

import scala.annotation.tailrec
import scala.collection.mutable


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

object Math {
  def even(n: Int) = n % 2 == 0

  def odd(n: Int) = !even(n)
}

case class Board(n: Int, emptyPosition: Board.Position, board: Map[Board.Position, Tile]) {
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

  private def inversions: Int = ???

  def solvable: Boolean = {
    import Math._
    val (x, _) = emptyPosition
    (odd(n) && even(inversions)) || (even(n) && (odd(inversions) && even(n - x)) || (even(inversions) && odd(n - x)))
  }

  def solved: Boolean = distance == 0

  def distance: Int = {
    val reversed = board.map { case (k, v) => v -> k }
    reversed.foldLeft(0) { case (acc, (t, (x, y))) =>
      val (a, b) = Board.goalPosition(n)(t)
      acc + math.abs(x - a) + math.abs(y - b)
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

  def move(ms: List[Move]): Option[Board] = {
    ms.foldLeft[Option[Board]](Some(this)) { case (Some(b), m) => b.move(m) }
  }

  def tick(moves: List[Move]): List[Board.State] = for {
    mv <- validMoves
    brd <- move(mv)
  } yield brd -> (mv :: moves)
}

object Board {
  type Position = (Int, Int)
  type State = (Board, List[Move])

  def apply(n: Int, tiles: List[Tile]): Board = {
    val board = tiles.zipWithIndex.map { case (t, i) => (i / n, i % n) -> t }
    val emptyPos = board.collect { case (p, Empty) => p }.head
    Board(n, emptyPos, board.toMap)
  }

  def goal(n: Int): Board = {
    val tiles: List[Tile] = (1 until n * n).map(Tile.Number).toList
    apply(n, tiles ++ (Empty :: Nil))
  }

  def goalPosition(n: Int)(t: Tile): Board.Position = t match {
    case Number(value) => ((value - 1) / n, (value - 1) % n)
    case Empty => (n - 1, n - 1)
  }

}


trait Solver[F[_]] extends (Board => F[List[Move]])


object Solver {

  implicit val boardOrdering: Ordering[Board] = (x: Board, y: Board) => {
    x.distance - y.distance
  }

  implicit val stateOrdering: Ordering[Board.State] = Ordering.by[Board.State, Board] { case (b, _) => b }.reverse

  def apply[F[_]](implicit F: MonadError[F, Throwable]): Solver[F] = (board: Board) => {

    lazy val queue = new mutable.PriorityQueue[Board.State]()
    queue.enqueue(board -> List.empty)

    @tailrec
    def loop(): F[List[Move]] = {
      if (queue.isEmpty) F.raiseError(Unsolvable)
      else {
        val (b, ms) = queue.dequeue()
        if (b.solved) F.pure(ms)
        else {
          b.tick(ms).foreach(queue.enqueue(_))
          loop()
        }
      }
    }

    F.map(loop())(_.reverse)
  }
}

