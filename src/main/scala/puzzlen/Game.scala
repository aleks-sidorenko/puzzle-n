package puzzlen

import cats.MonadError
import cats.data.State


sealed trait Move

object Move {

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

case class Board(n: Int, board: Map[Board.Position, Tile]) {
  def solved: Boolean = ???

  def validMoves: List[Move] = ???

  def move(m: Move): Board = ???
}

object Board {
  type Position = (Int, Int)

  def apply(n: Int, tiles: List[Tile]): Board =
    Board(n, tiles.zipWithIndex.map { case (t, i) => (i, i) -> t }.toMap)

  def goal(n: Int): Board = {
    val tiles = for {
      i <- 0 until n
      j <- 0 until n
      if i != n - 1 && j != n - 1
    } yield (i -> j) -> Tile.Number(i * n + j + 1)

    val all = tiles :+ (n - 1, n - 1) -> Tile.Empty

    Board(n, all.toMap)
  }
}


trait Solver[F[_]] extends (Board => F[List[Move]])

object Solver {

  def apply[F[_]](implicit F: MonadError[F, Throwable]): Solver[F] = (board: Board) => ???
}

