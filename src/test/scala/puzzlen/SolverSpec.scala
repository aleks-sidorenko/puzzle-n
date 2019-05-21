package puzzlen

import cats.Id
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{Checkers, PropertyChecks}


class SolverSpec extends FunSpec with Matchers with PropertyChecks with Checkers with TestHelpers {

  val solver = Solver[Id]

  val moves: Gen[List[Move]] =
    for {
      s <- Gen.choose(3, 20)
      lst <- Gen.listOfN(s, Gen.oneOf(Move.all.toList))
    } yield lst

  def backMoves(moves: List[Move]) = moves.map(_.back)

  val initialBoard: Gen[Board] =
    for {
      dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)
    } yield Board.goal(dim)

  val boardWithMoves: Gen[(Board, List[Move])] =
    for {
      num <- Gen.choose[Int](2, 15)
      board <- initialBoard
      (_, ms) = (1 to num).foldLeft[(Board, List[Move])](board -> List.empty) { case ((brd, ms), _) =>
        val mv = brd.validMoves.head
        brd.move(mv).get -> (mv :: ms)
      }
    } yield {
      board -> ms
    }


  describe("When specifying solvable initial board") {
    it("should solve correctly") {
      forAll(boardWithMoves) { case (brd, ms) =>
        solver(brd) shouldBe backMoves(ms)
      }
    }
  }

}
