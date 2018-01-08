package benchmarks
package cec2013
package niching

import scala.io.Source.fromResource

import shapeless._
import shapeless.ops.nat._

import benchmarks.dimension._
import benchmarks.matrix._

object Helpers {

  def shiftFromLine[N<:Nat:ToInt](line: String): Dimension[N,Double] = {
    val shift = line
      .trim
      .split("\\s+")
      .map(_.toDouble)
      .take(implicitly[ToInt[N]].apply)
      .toVector
    Sized.wrap(shift)
  }

  def shiftFromResource[N<:Nat:ToInt](resource: String): Dimension[N,Double] =
    shiftFromLine(fromResource(s"cec2013/niching/$resource").getLines.toList.head)

  def shiftsFromResource[N<:Nat:ToInt](resource: String): List[Dimension[N,Double]] =
    fromResource(s"cec2013/niching/$resource")
      .getLines
      .toList
      .map(shiftFromLine[N])

  def matrixFromLines[N<:Nat:ToInt](lines: List[String]) = {
    val dim = implicitly[ToInt[N]].apply
    val elements =
      lines
        .take(dim)
        .flatMap {
          _
            .trim
            .split("\\s+")
            .take(dim)
            .map(_.toDouble)
        }
        .grouped(dim)
        .map(_.toVector)
        .toVector

    Matrix.wrap[N,N,Double](elements: _*)
  }

  def matrixFromResource[N<:Nat:ToInt](resource: String) =
    matrixFromLines(fromResource(s"cec2013/niching/$resource").getLines.toList)

  def matricesFromResource[M<:Nat:ToInt,N<:Nat:ToInt](resource: String):
    Dimension[M,Matrix[N,N,Double]] = {
      val num = implicitly[ToInt[M]].apply
      val dim = implicitly[ToInt[N]].apply
      val matrices = fromResource(s"cec2013/niching/$resource")
        .getLines
        .sliding(dim)
        .toList
        .map(lines => matrixFromLines[N](lines.toList))

      Sized.wrap(matrices.toVector.take(num))
    }
}
