package puzzlen

import cats.{Eval, Id, Monad, MonadError}
import cats.syntax._
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.prop.{Checkers, PropertyChecks}
import puzzlen.PuzzleError.InvalidInput


trait TestHelpers {
  implicit val monadError: MonadError[Id, Throwable] = new MonadError[Id, Throwable] {
    private val M: Monad[Id] = implicitly[Monad[Id]]

    def pure[A](x: A): Id[A] =
      x

    def flatMap[A, B](fa: Id[A])(f: (A) => Id[B]): Id[B] =
      f(fa)

    def tailRecM[A, B](a: A)(f: (A) => Id[Either[A, B]]): Id[B] =
      M.tailRecM[A, B](a)(f)

    def raiseError[A](e: Throwable): Id[A] =
      throw e

    def handleErrorWith[A](fa: Id[A])(f: Throwable => Id[A]): Id[A] =
      try fa
      catch {
        case t: Throwable => f(t)
      }
  }
}

class AppSpec extends FunSpec with Matchers with PropertyChecks with Checkers with TestHelpers {

  def terminal(list: List[String]): Terminal[Id] = {

    new Terminal[Id] {
      var output: List[String] = List.empty
      val input = list.toVector
      var i = 0

      def putLn(value: String) = {
        output = value :: output
        ()
      }

      def readLn = {
        if (i < input.length) {
          val ret = input(i)
          i += 1
          ret
        } else ""
      }
    }

  }


  val app = new AppF[Id] {
    implicit val F: MonadError[Id, Throwable] = monadError

  }

  describe("When specifying wrong input") {

    describe("When dimension is not integer") {

      it("should produce invalid input error") {

        forAll(Gen.listOfN(2, Gen.alphaStr)) { input =>
          val term = terminal(input)
          a[InvalidInput] should be thrownBy {
            app.main(List.empty)(term, Solver[Id], Parser[Id])
          }
        }
      }
    }

    describe("When dimension is integer") {

      describe("and dimension is out of allowed range") {
        it("should produce invalid input error") {

          val gen = Gen.oneOf("1" :: Nil, "11" :: Nil, "15" :: Nil, "-5" :: Nil)

          forAll(gen) { input =>
            val term = terminal(input)
            a[InvalidInput] should be thrownBy {
              app.main(List.empty)(term, Solver[Id], Parser[Id])
            }
          }
        }
      }
      describe("and board number of rows is incorrect") {
        it("should produce invalid input error") {

          val gen = Gen.oneOf("4" :: "1 2 3 4" :: Nil, "3" :: "1 2 3" :: "3 4 5" :: Nil)

          forAll(gen) { input =>
            val term = terminal(input)
            a[InvalidInput] should be thrownBy {
              app.main(List.empty)(term, Solver[Id], Parser[Id])
            }
          }
        }
      }

      describe("and board rows length are incorrect") {
        it("should produce invalid input error") {

          val gen = Gen.oneOf("3" :: "1 2 3" :: "5 6 7 8" :: "9 10 11" :: Nil, "2" :: "1 2" :: "3 X 5" :: Nil)

          forAll(gen) { input =>
            val term = terminal(input)
            a[InvalidInput] should be thrownBy {
              app.main(List.empty)(term, Solver[Id], Parser[Id])
            }
          }
        }
      }
    }


  }


}
