package benchmarks
package cec2013
package niching

import org.scalacheck._
import org.scalacheck.Prop._
import shapeless._
import spire.implicits._

import cilib.RNG

import benchmarks.dimension._
import benchmarks.dimension.Generators._
import benchmarks.implicits._
import benchmarks.matrix._
import Benchmarks._

object BenchmarksTest extends Properties("CEC2013 Niching Benchmarks") {

  // property("f8") = forAll(gen2(0.0, 1.0)) { g =>
  //   f8(g) >= -40.0 &&
  //   f8(g) <= 0.0
  // }
  //
  // property("f9") = forAll(gen2(-5.0, 5.0)) { g =>
  //   implicit val F9Params = new F9Params[nat._2,Double] {
  //     val params = (
  //       Sized.wrap[IndexedSeq[Dimension[nat._2,Double]],nat._6](
  //         Vector.fill(6)(Sized(0.0, 0.0))
  //       ),
  //       -2500.0
  //     )
  //   }
  //   f9(g) >= -2500.0 &&
  //   f9(g) <= 0.0
  // }
  //
  // property("f10") = forAll(gen2(-5.0, 5.0)) { g =>
  //   implicit val F10Params = new F10Params[nat._2,Double] {
  //     val params = (
  //       Sized.wrap[IndexedSeq[Dimension[nat._2,Double]],nat._8](
  //         Vector.fill(8)(Sized(0.0, 0.0))
  //       ),
  //       -2500.0
  //     )
  //   }
  //   f10(g) >= -2500.0 &&
  //   f10(g) <= 0.0
  // }

  property("f11") = forAll(gen2(-5.0, 5.0)) { g =>
    implicit val F11Params = new F11Params[nat._2,Double] {
      val params = (
        Sized.wrap[IndexedSeq[Dimension[nat._2,Double]],nat._6](
          Vector.fill(6)(Sized(0.0, 0.0))
        ),
        Sized.wrap[IndexedSeq[Matrix[nat._2,nat._2,Double]],nat._6](
          Vector.fill(6)(Matrix.rotation[nat._2]).map(_ eval RNG.fromTime),
        ),
        -4000.0
      )
    }
    val v = f11(g)
    if (v < -4000.0 || v > 0.0) {
      println(v)
    }
    v >= -4000.0 &&
    v <= 0.0
  }

  // property("f12") = forAll(gen2(-5.0, 5.0)) { g =>
  //   implicit val F12Params = new F12Params[nat._2,Double] {
  //     val params = (
  //       Sized.wrap[IndexedSeq[Dimension[nat._2,Double]],nat._8](
  //         Vector.fill(8)(Sized(0.0, 0.0))
  //       ),
        // Sized.wrap[IndexedSeq[Matrix[nat._2,nat._2,Double]],nat._6](
        //   Vector.fill(6)(Matrix.rotation[nat._2]).map(_ eval RNG.fromTime),
        // ),
  //       -4000.0
  //     )
  //   }
  //   f12(g) >= -4000.0 &&
  //   f12(g) <= 0.0
  // }
}
