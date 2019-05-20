package puzzlen

import cats.MonadError
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.util.Try


trait AppF[F[_]] {

  implicit def F: MonadError[F, Throwable]

  private def input()(implicit term: Terminal[F], parser: Parser[F]): F[Board] = {
    val (min, max) = (2, 10)

    for {
      _ <- term.putLn(s"Please, enter the dimension of puzzle ($min-$max):")
      dim <- term.readLn
      d <- Try(dim.toInt).map(F.pure(_)).getOrElse(F.raiseError(PuzzleError.InvalidInput(s"'$dim' should be integer")))

      _ <- F.ifM(F.pure(d >= min && d <= max))(
        ifTrue = F.pure(()),
        ifFalse = F.raiseError(PuzzleError.InvalidInput(s"'$dim' should be integer")))
      _ <- term.putLn("Please, enter the puzzle board of dimension ${dim} row by row (use 'X' for empty tile):")
      lines <- (1 to d).map(_ => term.readLn).toList.sequence
      board <- parser.parse(lines)
    } yield board
  }


  private def program()(implicit term: Terminal[F], parser: Parser[F], solver: Solver[F]): F[Unit] = {
    for {
      board <- input()
      solution <- solver(board)
      _ <- solution.map(m => term.putLn(m.toString)).sequence
    } yield ()
  }

  def main(args: List[String])(implicit term: Terminal[F], solver: Solver[F], parser: Parser[F]): F[Unit] = {
    program().handleErrorWith({
      case PuzzleError.Unsolvable => term.putLn("The puzzle is not solvable")
      case PuzzleError.InvalidInput(err) => term.putLn(s"The input is not correct: $err")
      case err => term.putLn(s"Unknown error: ${err.getMessage}")
    })
  }
}

object App extends AppF[IO] with IOApp {

  implicit val F = MonadError[IO, Throwable]


  def run(args: List[String]): IO[ExitCode] = {

    main(args)(Terminal[IO], Solver[IO], Parser[IO]).value.as(ExitCode.Success)
  }
}