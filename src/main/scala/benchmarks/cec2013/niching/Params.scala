package benchmarks
package cec2013
package niching

import shapeless._
import benchmarks.dimension._
import benchmarks.matrix._

trait F8Params  [N<:Nat]   { val params: Dimension[N,Int] }
trait F9Params  [N<:Nat,A] { val params: (Dimension6[Dimension[N,A]], A) }
trait F10Params [N<:Nat,A] { val params: (Dimension8[Dimension[N,A]], A) }
trait F11Params [N<:Nat,A] {
  val params: (Dimension6[Dimension[N,A]], Dimension6[Matrix[N,N,A]], A)
}
trait F12Params [N<:Nat,A] {
  val params: (Dimension8[Dimension[N,A]], Dimension8[Matrix[N,N,A]], A)
}

trait Params {
  implicit val f8Params2 = new F8Params[nat._2] {
    val params = Sized(3, 4)
  }
  implicit val f8Params16 = new F8Params[nat._16] {
    val params = Sized(
      1, 1, 1, 2, 1, 1, 1, 2, 1, 1, 1, 3, 1, 1, 1, 4
    )
  }
}
