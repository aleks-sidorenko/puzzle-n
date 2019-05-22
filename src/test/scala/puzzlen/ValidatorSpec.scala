package puzzlen

import cats.Id
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{Checkers, PropertyChecks}
import puzzlen.PuzzleError.ValidationError


class ValidatorSpec extends FunSpec with Matchers with PropertyChecks with Checkers with TestHelpers {

  val validator = BoardValidator[Id]


  val boardNoEmpty: Gen[(Int, List[Tile])] =
    for {
      dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)

    } yield (dim, (0 until dim * dim).toList.map(Tile.Number))

  val boardLessNums: Gen[(Int, List[Tile])] =
    for {
      dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)
    } yield (dim, Tile.Empty :: (0 until dim * dim - 1).toList.map(Tile.Number))

  val boardMoreNums: Gen[(Int, List[Tile])] =
    for {
      dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)
    } yield (dim, Tile.Empty :: (0 until dim * dim + 1).toList.map(Tile.Number))

  val boardWrongNums: Gen[(Int, List[Tile])] =
    for {
      dim <- Gen.choose[Int](BoardParser.min, BoardParser.max)
    } yield (dim, Tile.Empty :: (0 until dim * dim).toList.map(x => Tile.Number(x + 1)))

  val boardWrong = Gen.oneOf(boardLessNums, boardWrongNums, boardNoEmpty, boardMoreNums)


  describe("When specifying not valid board without empty") {
    it("should return validation error") {
      forAll(boardWrong) { case (dim, tiles) =>
        a[ValidationError.type] should be thrownBy {
          val brd = Board(dim, tiles)
          validator.validate(brd)
        }

      }
    }
  }

}
