package cilib
package benchmarks

import Sized._

import org.scalacheck._
import org.scalacheck.Gen

import scalaz.{Apply,NonEmptyList}
import scalaz.std.list._
import scalaz.syntax.apply._
import scalaz.scalacheck.ScalaCheckBinding._

object Generators {

  def gen1(l: Double, u: Double) =
    Gen.choose(l, u)

  def gen2(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple2.apply }

  def gen2D(d1: (Double, Double), d2: (Double, Double)) =
    (Gen.choose(d1._1, d1._2) |@| Gen.choose(d2._1, d2._2)) { Tuple2.apply }

  def gen3(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple3.apply }

  def gen3D(d1: (Double, Double), d2: (Double, Double), d3: (Double, Double)) =
    (Gen.choose(d1._1, d1._2) |@| Gen.choose(d2._1, d2._2) |@| Gen.choose(d3._1, d3._2)) { Tuple3.apply }

  def gen4(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l,u)) { Tuple4.apply }

  def gen5(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple5.apply }

  def gen6(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple6.apply }

  def gen10(l: Double, u: Double) =
    (Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@|
     Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u) |@| Gen.choose(l, u)) { Tuple10.apply }

  def gen1And(l: Double, u: Double) =
    (gen1(l, u) |@| Gen.containerOf[List, Double](gen1(l, u))) { (a, b) => Sized1And(a, b) }

  def gen2And(l: Double, u: Double) =
    (gen2(l, u) |@| Gen.containerOf[List, Double](gen1(l, u))) { (a, b) => Sized2And(a._1, a._2, b) }

  def genNEL(l: Double, u: Double) =
    gen1And(l, u).map(x => NonEmptyList.nel(x.head, x.tail))

  def genConst(v: Double) = genNEL(v, v)

  def genMultipleOf3[A](gen: Gen[A]) =
    (gen3Generic(gen) |@| Gen.containerOf[List,Sized3[A]](gen3Generic(gen))) { (a, b) => MultipleOf3(a, b) }

  def genMultipleOf5[A](gen: Gen[A]) =
    (gen5Generic(gen) |@| Gen.containerOf[List,Sized5[A]](gen5Generic(gen))) { (a, b) => MultipleOf5(a, b) }

  def genMultipleOf6[A](gen: Gen[A]) =
    (gen6Generic(gen) |@| Gen.containerOf[List,Sized6[A]](gen6Generic(gen))) { (a, b) => MultipleOf6(a, b) }

  def gen3Generic[A](gen: Gen[A]): Gen[Sized3[A]] =
    (gen |@| gen |@| gen) { Tuple3.apply }

  def gen5Generic[A](gen: Gen[A]): Gen[Sized5[A]] =
    (gen |@| gen |@| gen |@| gen |@| gen) { Tuple5.apply }

  def gen6Generic[A](gen: Gen[A]): Gen[Sized6[A]] =
    (gen |@| gen |@| gen |@| gen |@| gen |@| gen) { Tuple6.apply}

}
