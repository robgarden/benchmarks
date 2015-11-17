package cilib
package benchmarks

import scalaz.Foldable
import scalaz.syntax.foldable._

import Sized._

object DiscreteBenchmarks {

  def oneMax[F[_]: Foldable](x: F[Boolean]) =
    x.foldLeft(0)((xi, xi1) => if (xi1) xi + 1 else xi)

  def order3Deceptive[F[_]: Foldable](x: MultipleOf3[F,Boolean]) = {
    def value(a: Sized3[Boolean]) = a.productIterator.filter(_ == true).size match {
      case 0 => 0.9
      case 1 => 0.6
      case 2 => 0.3
      case 3 => 1.0
    }
    x.foldLeft(0.0)((xi, xi1) => xi + value(xi1))
  }

  def order3Biploar[F[_]: Foldable](x: MultipleOf6[F,Boolean]) = {
    def value(a: Sized6[Boolean]) = a.productIterator.filter(_ == true).size match {
      case 0 | 6 => 1.0
      case 1 | 5 => 0.0
      case 2 | 4 => 0.4
      case 3     => 0.8
    }
    x.foldLeft(0.0)((xi, xi1) => xi + value(xi1))
  }

  def order5[F[_]: Foldable](x: MultipleOf5[F,Boolean]) = {
    def value(a: Sized5[Boolean]) = a match {
      case (false, false, false, false, false) => 4.0
      case (false, false, false, false, true)  => 3.0
      case (false, false, false, true,  true)  => 2.0
      case (false, false, true,  true,  true)  => 1.0
      case (true,  true,  true,  true,  true)  => 3.5
      case _  => 0.0
    }
    x.foldLeft(0.0)((xi, xi1) => xi + value(xi1))
  }
}
