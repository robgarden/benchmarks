package cilib

import scalaz.{Foldable,Id,OneAnd}
import scalaz.syntax.apply._
import scalaz.syntax.foldable1._
import scalaz.std.option._

object Sized {

  type Sized1[A] = Id.Id[A]
  type Sized2[A] = (A, A)
  type Sized3[A] = (A, A, A)
  type Sized4[A] = (A, A, A, A)
  type Sized5[A] = (A, A, A, A, A)
  type Sized6[A] = (A, A, A, A, A, A)
  type Sized10[A] = (A, A, A, A, A, A, A, A, A, A)

  final case class Sized1And[F[_]: Foldable, A](a: A, rest: F[A]) {
    lazy val toList = a :: rest.toList
    lazy val head = a
    lazy val tail = rest
  }

  final case class Sized2And[F[_]: Foldable, A](a: A, b: A, rest: F[A]) {
    lazy val toList = a :: b :: rest.toList
  }

  final case class MultipleOf3[F[_]: Foldable, A](a: Sized3[A], rest: F[Sized3[A]]) {
    lazy val toList = a :: rest.toList
    lazy val size = toList.size * 3
    def foldLeft[B](b: => B)(f: (B, Sized3[A]) => B) = toList.foldLeft(b)(f)
  }

  final case class MultipleOf5[F[_]: Foldable, A](a: Sized5[A], rest: F[Sized5[A]]) {
    lazy val toList = a :: rest.toList
    lazy val size = toList.size * 5
    def foldLeft[B](b: => B)(f: (B, Sized5[A]) => B) = toList.foldLeft(b)(f)
  }

  final case class MultipleOf6[F[_]: Foldable, A](a: Sized6[A], rest: F[Sized6[A]]) {
    lazy val toList = a :: rest.toList
    lazy val size = toList.size * 6
    def foldLeft[B](b: => B)(f: (B, Sized6[A]) => B) = toList.foldLeft(b)(f)
  }

  def toSized1[F[_]: Foldable, A](x: F[A]): Option[Sized1[A]] = x.index(0)

  def toSized2[F[_]: Foldable, A](x: F[A]): Option[Sized2[A]] =
    (x.index(0) |@| x.index(1)) { (_, _) }

  def toSized3[F[_]: Foldable, A](x: F[A]): Option[Sized3[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2)) { (_, _, _) }

  def toSized4[F[_]: Foldable, A](x: F[A]): Option[Sized4[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3)) { (_, _, _, _) }

  def toSized5[F[_]: Foldable, A](x: F[A]): Option[Sized5[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4)) { (_, _, _, _, _) }

  def toSized6[F[_]: Foldable, A](x: F[A]): Option[Sized6[A]] =
    (x.index(0) |@| x.index(1) |@| x.index(2) |@| x.index(3) |@| x.index(4) |@| x.index(5)) {
      (_, _, _, _, _, _)
    }

}
