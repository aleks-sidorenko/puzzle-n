package puzzlen

import cats.MonadError
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._


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


  private def program()(implicit term: Terminal[F], parser: BoardParser[F],
                        validator: BoardValidator[F], solver: Solver[F]): F[Unit] = {
    for {
      board <- input()
      _ <- validator.validate(board)
      solution <- solver(board)
      _ <- solution.map(m => term.putLn(m.toString)).sequence
    } yield ()
  }

  def main(args: List[String])(implicit term: Terminal[F], parser: BoardParser[F],
                               validator: BoardValidator[F], solver: Solver[F]): F[Unit] = {
    program().handleErrorWith({
      case PuzzleError.Unsolvable => term.putLn("The puzzle is not solvable")
      case PuzzleError.ValidationError => term.putLn(s"The puzzle with this input is invalid")
      case PuzzleError.ParsingError(err) => term.putLn(s"The input is not correct: $err")
      case err => term.putLn(s"Unknown error: ${err.getMessage}")
    })
  }
}

object App extends AppF[IO] with IOApp {

  implicit val F = MonadError[IO, Throwable]


  def run(args: List[String]): IO[ExitCode] = {

    import BoardParser._
    main(args)(Terminal[IO], BoardParser[IO], BoardValidator[IO], Solver[IO]).as(ExitCode.Success)
  }
}