package puzzlen

import cats.MonadError
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._

import scala.util.Try


trait AppF[F[_]] {

  implicit def F: MonadError[F, Throwable]

  private def input()(implicit term: Terminal[F], parser: BoardParser[F]): F[Board] = {

    for {
      _ <- term.putLn(s"Please, enter the dimension of puzzle (${BoardParser.min}-${BoardParser.max}):")
      dim <- term.readInt
      _ <- term.putLn(s"Please, enter the board of size $dim row by row (empty tile is '0'):")
      lines <- (1 to dim).map(_ => term.readLn).toList.sequence
      board <- parser.parse((dim :: lines).mkString(" "))
    } yield board
  }


  private def program()(implicit term: Terminal[F], parser: BoardParser[F], solver: Solver[F]): F[Unit] = {
    for {
      board <- input()
      solution <- solver(board)
      _ <- solution.map(m => term.putLn(m.toString)).sequence
    } yield ()
  }

  def main(args: List[String])(implicit term: Terminal[F], solver: Solver[F], parser: BoardParser[F]): F[Unit] = {
    program().handleErrorWith({
      case PuzzleError.Unsolvable => term.putLn("The puzzle is not solvable")
      case PuzzleError.ParsingError(err) => term.putLn(s"The input is not correct: $err")
      case err => term.putLn(s"Unknown error: ${err.getMessage}")
    })
  }
}

object App extends AppF[IO] with IOApp {

  implicit val F = MonadError[IO, Throwable]


  def run(args: List[String]): IO[ExitCode] = {

    main(args)(Terminal[IO], Solver[IO], BoardParser[IO]).as(ExitCode.Success)
  }
}