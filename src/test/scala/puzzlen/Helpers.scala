package puzzlen

import cats.{Id, Monad, MonadError}

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