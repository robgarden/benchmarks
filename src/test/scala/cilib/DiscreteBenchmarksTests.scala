package cilib
package benchmarks

import DiscreteBenchmarks._
import Generators._
import Sized._

import org.scalacheck._
import org.scalacheck.Prop._

import scalaz.std.list._

import spire.implicits._

object DiscreteBenchmarksTest extends Properties("DiscreteBenchmarks") {

  property("oneMax") = forAll { (g: List[Boolean]) =>
    oneMax(g) >= 0 &&
    oneMax(g) <= g.length
  } && {
    oneMax(List(true, false, true)) === 2 &&
    oneMax(List(true, false, false)) === 1
  }

  property("order3Deceptive") = forAll(genMultipleOf3(Arbitrary.arbitrary[Boolean])) { g =>
    order3Deceptive(g) >= 0.0 &&
    order3Deceptive(g) <= g.size / 3.0
  } && {
    order3Deceptive(MultipleOf3((true, false, true), List[Sized3[Boolean]]())) === 0.3 &&
    order3Deceptive(MultipleOf3((true, false, false), List[Sized3[Boolean]]())) === 0.6
  }

  property("order3Biploar") = forAll(genMultipleOf6(Arbitrary.arbitrary[Boolean])) { g =>
    order3Biploar(g) >= 0.0 &&
    order3Biploar(g) <= g.size / 6.0
  } && {
    order3Biploar(MultipleOf6((true, false, true, true, false, true), List[Sized6[Boolean]]())) === 0.4 &&
    order3Biploar(MultipleOf6((true, false, false, false, false, false), List[Sized6[Boolean]]())) === 0.0
  }

  property("order5") = forAll(genMultipleOf5(Arbitrary.arbitrary[Boolean])) { g =>
    order5(g) >= 0.0 &&
    order5(g) <= (g.size / 5) * 4.0
  } && {
    order5(MultipleOf5((true, false, true, true, false), List[Sized5[Boolean]]())) === 0.0 &&
    order5(MultipleOf5((false, false, false, true, true), List[Sized5[Boolean]]())) === 2.0
  }
}
