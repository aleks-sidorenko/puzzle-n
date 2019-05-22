package puzzlen

import cats.Id
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{Checkers, PropertyChecks}
import puzzlen.PuzzleError.{Unsolvable, ValidationError}
import puzzlen.Tile.Empty


class SolverSpec extends FunSpec with Matchers with PropertyChecks with Checkers with TestHelpers {

  val solver = Solver[Id]

  val moves: Gen[List[Move]] =
    for {
      s <- Gen.choose(3, 20)
      lst <- Gen.listOfN(s, Gen.oneOf(Move.all.toList))
    } yield lst

  val initialBoard: Gen[Board] =
    for {
      dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)
    } yield Board.goal(dim)

  val boardWithMoves: Gen[(Board, List[Move])] =
    for {
      num <- Gen.choose[Int](2, 10)
      board <- initialBoard
      (brd, ms) = (1 to num).foldLeft[(Board, List[Move])](board -> List.empty) { case ((brd, ms), _) =>
        val mv = brd.validMoves.head
        brd.move(mv).get -> (mv :: ms)
      }
    } yield {
      brd -> ms.reverse
    }

  val randomBoard: Gen[Board] = for {
    dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)
    tiles = scala.util.Random.shuffle(Tile.Empty :: (0 until dim * dim - 1).toList.map(Tile.Number))
    brd = Board(dim, tiles)
  } yield brd

  val unsolvableBoard: Gen[Board] = randomBoard.filterNot(_.solvable)

  val customBoard: Gen[Board] = Gen.oneOf(Seq(
    Board(3, (1 :: 2 :: 3 :: 5 :: 4 :: 6 :: 8 :: 7 :: Nil).map(Tile.Number) ::: Empty :: Nil)
  ))


  describe("When specifying solvable initial board") {
    it("should solve correctly") {
      forAll(boardWithMoves) { case (brd, ms) =>
        brd.solvable shouldBe true
        val goal = Board.goal(brd.n)
        val res = solver(brd)
        // first we check if apply all moves to goal board we will get generated board
        goal.move(ms) shouldBe Some(brd)
        // check that if apply all result moves to board we will get goal board
        brd.move(res) shouldBe Some(goal)
      }
    }
  }

  describe("When specifying custom initial board") {
    it("should solve correctly") {
      forAll(customBoard) { case (brd) =>
        brd.solvable shouldBe true
        val goal = Board.goal(brd.n)
        val res = solver(brd)

        // check that if apply all result moves to board we will get goal board
        brd.move(res) shouldBe Some(goal)
      }
    }
  }


  describe("When specifying not solvable initial board") {
    it("should produce unsolvable error") {

      forAll(unsolvableBoard) { (brd) =>
        brd.solvable shouldBe false
        a[Unsolvable.type] should be thrownBy {
          solver(brd)
        }
      }
    }
  }

}
