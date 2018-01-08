package benchmarks
package cec
package cec2013
package niching

import shapeless._
import shapeless.ops.nat._
import spire.algebra._
import spire.implicits._
import spire.math._

import benchmarks.Benchmarks._
import benchmarks.cec.cec2005.Benchmarks.f8f2
import benchmarks.dimension._
import benchmarks.matrix._
import benchmarks.implicits._

object Benchmarks {

  // Five Uneven Peak Trap
  def f1[A:Field:Order] = fiveUnevenPeakTrap[A] _

  // Equal Maxima
  def f2[A:Field:Trig] = equalMaxima[A] _

  // Uneven Decreasing Maxima
  def f3[A:Field:NRoot:Trig] = unevenDecreasingMaxima[A] _

  // Himmelblau
  def f4[A:Ring] = himmelblau[A] _

  // Six Hump Camel Back
  def f5[A:Field] = sixHumpCamelback[A] _

  // Shubert
  def f6[N<:Nat,A:Field:Trig] = shubert[N,A] _

  // Vincent
  def f7[N<:Nat,A:Field:Trig] = vincent[N,A] _

  // Modified Rastrigin - All Global Optima
  def f8[N<:Nat,A:Field:Trig](x: Dimension[N,A])(implicit P: F8Params[N]) =
    -(x zip P.params).mapSum { case (xi, ki) =>
      10.0 + 9.0 * cos(2.0 * pi * ki * xi)
    }

  private[this] def hybrid[M<:Nat:ToInt,N<:Nat:ToInt,A:Field:Signed:Trig:Ordering]
    (o: Dimension[M,Dimension[N,A]],
     m: Dimension[M,Matrix[N,N,A]],
     f: Dimension[M,Dimension[N,A] => A],
     λ: Dimension[M,Double],
     σ: Dimension[M,Double]
   ): Dimension[N,A] => A = {
    val bias: Dimension[M,Int] = Sized.wrap(Vector.fill(implicitly[ToInt[M]].apply)(0))
    val C = 2000.0
    val D = implicitly[ToInt[N]].apply
    val fmax = (f zip λ zip m) map { case ((fi, λi), mi) =>
      val temp: Dimension[N,Double] = Sized.wrap(Vector.fill(D)(5.0 / λi))
      val point = temp.map(implicitly[Field[A]].fromDouble) rotate mi
      abs(fi(point))
    }
    x => {
      val zipped = (o zip m zip f zip λ zip σ zip bias zip fmax) map {
        case ((((((oi, mi), fi), λi), σi), bi), fmaxi) =>
          val zi = (x shift oi).map(_ / λi) rotate mi
          (oi, mi, fi, λi, σi, bi, fmaxi, zi)
      }
      val weights = zipped map {
        case (oi, _, _, _, σi, _, _, zi) =>
          val denom = (x zip oi) mapSum { case (xk, oik) => (xk - oik) ** 2 }
          exp(-denom / (2.0 * zi.size * σi * σi))
      }

      val maxWeight = weights.max
      val w1mMaxPow = 1.0 - (maxWeight ** 10)
      val adjustedWeights = weights map { wi =>
        if (wi != maxWeight) wi * w1mMaxPow
        else wi
      }
      val wSum = adjustedWeights mapSum (xi => xi)
      // normalize the weights
      val normWeights = adjustedWeights map { _ / wSum }

      (zipped zip normWeights) mapSum {
        case ((_, _, fi, _, _, bi, fmaxi, zi), wi) =>
          wi * ((C * fi(zi) / fmaxi) + bi)
      }
    }
  }

  // Composition Function 1
  def f9[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Ordering:Signed:Trig]
    (x: Dimension[N,A])(implicit P: F9Params[N,A]): A =
      P.params match {
        case (o, fbias) => {
          val f: Dimension6[Dimension[N,A] => A] = Sized(
            griewank[N,A] _,
            griewank[N,A] _,
            weierstrass[N,A] _,
            weierstrass[N,A] _,
            spherical[N,A] _,
            spherical[N,A] _
          )
          val m = Sized.wrap[IndexedSeq[Matrix[N,N,A]],nat._6](Vector.fill(6)(Matrix.eye[N,A]))
          val λ = Sized(1.0, 1.0, 8.0, 8.0, 0.2, 0.2)
          val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
          val h = hybrid[nat._6,N,A](o, m, f, λ, σ)
          h(x) + fbias
        }
      }

  // Composition Function 2
  def f10[N<:Nat:GTEq1:ToInt,A:Field:NRoot:Ordering:Signed:Trig]
    (x: Dimension[N,A])(implicit P: F10Params[N,A]): A =
      P.params match {
        case (o, fbias) => {
          val f: Dimension8[Dimension[N,A] => A] = Sized(
            rastrigin[N,A] _,
            rastrigin[N,A] _,
            weierstrass[N,A] _,
            weierstrass[N,A] _,
            griewank[N,A] _,
            griewank[N,A] _,
            spherical[N,A] _,
            spherical[N,A] _
          )
          val m = Sized.wrap[IndexedSeq[Matrix[N,N,A]],nat._8](Vector.fill(8)(Matrix.eye[N,A]))
          val λ = Sized(1.0, 1.0, 10.0, 10.0, 0.1, 0.1, 1.0 / 7.0, 1.0 / 7.0)
          val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)
          val h = hybrid[nat._8,N,A](o, m, f, λ, σ)
          h(x) + fbias
        }
      }

  // Composition Function 3
  def f11[N<:Nat:GTEq2:HasHead:ToInt,A:Field:NRoot:Ordering:Signed:Trig]
    (x: Dimension[N,A])(implicit P: F11Params[N,A]): A =
      P.params match {
        case (o, m, fbias) => {
          val f: Dimension6[Dimension[N,A] => A] = Sized(
            f8f2[N,A] _,
            f8f2[N,A] _,
            weierstrass[N,A] _,
            weierstrass[N,A] _,
            griewank[N,A] _,
            griewank[N,A] _
          )
          val λ = Sized(0.25, 0.1, 2.0, 1.0, 2.0, 5.0)
          val σ = Sized(1.0, 1.0, 2.0, 2.0, 2.0, 2.0)
          val h = hybrid[nat._6,N,A](o, m, f, λ, σ)
          h(x) + fbias
        }
      }

  // Composition Function 4
  def f12[N<:Nat:GTEq2:HasHead:ToInt,A:Field:NRoot:Ordering:Signed:Trig]
    (x: Dimension[N,A])(implicit P: F12Params[N,A]): A =
      P.params match {
        case (o, m, fbias) => {
          val f: Dimension8[Dimension[N,A] => A] = Sized(
            rastrigin[N,A] _,
            rastrigin[N,A] _,
            f8f2[N,A] _,
            f8f2[N,A] _,
            weierstrass[N,A] _,
            weierstrass[N,A] _,
            griewank[N,A] _,
            griewank[N,A] _
          )
          val λ = Sized(4.0, 1.0, 4.0, 1.0, 0.1, 0.2, 0.1, 1.0 / 40.0)
          val σ = Sized(1.0, 1.0, 1.0, 1.0, 1.0, 2.0, 2.0, 2.0)
          val h = hybrid[nat._8,N,A](o, m, f, λ, σ)
          h(x) + fbias
        }
      }
}
