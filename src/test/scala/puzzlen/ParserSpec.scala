package puzzlen

import cats.Id
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{Checkers, PropertyChecks}
import puzzlen.PuzzleError.ParsingError


class ParserSpec extends FunSpec with Matchers with PropertyChecks with Checkers with TestHelpers {

  val parser = BoardParser[Id]

  def raw(f: Int => Gen[List[String]]): Gen[(Int, String)] = {
    for {
      s <- Gen.choose(BoardParser.min, BoardParser.max)
      lst <- f(s)
      g = scala.util.Random.shuffle(lst).mkString(" ")
    } yield s -> s"$s $g"
  }

  describe("When specifying wrong input") {

    describe("and tiles are not numbers") {

      it("should produce parsing error") {

        val gen = raw(s => Gen.listOfN(s, Gen.alphaChar.map(_.toString)))

        forAll(gen) { case (_, raw) =>
          a[ParsingError] should be thrownBy {
            parser.parse(raw)
          }
        }
      }

    }

    describe("and tiles number are less than dimension") {

      it("should produce parsing error") {

        val gen = raw(s => Gen.listOfN(s / 2, Gen.numChar.map(_.toString)))

        forAll(gen) { case (_, raw) =>
          a[ParsingError] should be thrownBy {
            parser.parse(raw)
          }
        }
      }

    }

  }

  describe("When specifying correct input") {

    it("should return board") {

      val gen = raw(s => Gen.const((0 until s * s).map(_.toString).toList))

      forAll(gen) { case (n, raw) =>
        parser.parse(raw) should matchPattern { case Board(i, _, _) if i == n => }
      }
    }

  }

}
