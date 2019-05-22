package puzzlen

import cats.MonadError
import puzzlen.PuzzleError.{Unsolvable, ValidationError}
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


sealed trait Tile {
  def num: Int =
    this match {
      case Empty => 0
      case Number(v) => v
    }
}

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

  lazy val tiles: Vector[Tile] = (for {
    i <- 0 until n
    j <- 0 until n
  } yield board(i -> j)).toVector

  lazy val tilesSet = tiles.toSet

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

  private lazy val inversions: Int = {
    def inversion(i: Int, j: Int): Boolean = (tiles(i), tiles(j)) match {
      case (Number(a), Number(b)) => a > b
      case _ => false
    }

    (for {
      i <- 0 until n * n
      j <- i + 1 until n * n
      if inversion(i, j)
    } yield 1).sum
  }

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

  def repr: String = tiles.map(_.num.toString).grouped(n).map(_.mkString(" ")).mkString("\n")
}

object Board {
  type Position = (Int, Int)
  type State = (Board, List[Move])

  def apply(n: Int, tiles: List[Tile]): Board = {
    if (tiles.length != n * n) throw ValidationError
    val board = tiles.zipWithIndex.map { case (t, i) => (i / n, i % n) -> t }
    val emptyPos = board.collectFirst { case (p, Empty) => p }.getOrElse(throw ValidationError)
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

  class SolverState()(implicit ord: Ordering[Board.State]) {
    // the min heap data structure to get next board state with less distance to goal and with minimum moves done
    private lazy val queue = new mutable.PriorityQueue[Board.State]()(ord)
    // we need to keep track of already visited boards to ignore them further
    private lazy val seen = new mutable.HashSet[Board]()

    private def has(b: Board): Boolean = seen.contains(b)

    def isEmpty = queue.isEmpty

    def put(s: Board.State): Unit = {
      val (b, ms) = s
      if (!has(b)) {
        queue.enqueue(s)
        seen += b
      }
    }

    def get: Board.State = queue.dequeue()

  }

  implicit val boardOrdering: Ordering[Board] = (x: Board, y: Board) => {
    y.distance - x.distance
  }

  implicit val movesOrdering: Ordering[List[Move]] = Ordering.by(-1 * _.length)

  // our state is ordered by minimum distance to goal and length of moves done
  implicit val stateOrdering: Ordering[Board.State] = Ordering.Tuple2(boardOrdering, movesOrdering)

  def apply[F[_]](implicit F: MonadError[F, Throwable]): Solver[F] = (board: Board) => {

    if (!board.solvable) F.raiseError(Unsolvable)
    else {
      val state = new SolverState()

      state.put(board -> List.empty)

      @tailrec
      def loop(): F[List[Move]] = {
        if (state.isEmpty) F.raiseError(Unsolvable)
        else {
          val (b, ms) = state.get
          if (b.solved) F.pure(ms)
          else {
            b.tick(ms).foreach(state.put)
            loop()
          }
        }
      }

      F.map(loop())(_.reverse)
    }

  }

}