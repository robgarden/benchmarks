package benchmarks
package cec

import scala.io.Source.fromResource

import scalaz.Scalaz._

import shapeless._
import shapeless.ops.nat._

import dimension._
import matrix._
import benchmarks.implicits._

case class Helper(prefix: String) {

  def fbiasFromResource(fNumber: Int) =
    fromResource(s"${prefix}/fbias_data.txt")
      .mkString
      .trim
      .split("\\s+")
      .toList.toNel
      .flatMap { _.index(fNumber - 1) }
      .get.toDouble

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
    shiftFromLine(fromResource(s"${prefix}/$resource").getLines.toList.head)

  def shiftsFromResource[N<:Nat:ToInt](resource: String): List[Dimension[N,Double]] =
    fromResource(s"${prefix}/$resource")
      .getLines
      .toList
      .map(shiftFromLine[N])

  def shiftFromResourceF[N<:Nat:ToInt](resource: String, f: List[String] => String) =
    (f andThen shiftFromLine[N])(fromResource(s"${prefix}/$resource").getLines.toList)

  def matrixFromLines[N<:Nat](lines: List[String])(implicit ev: ToInt[N]) = {
    val dim = ev.apply
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
    matrixFromLines(fromResource(s"${prefix}/$resource").getLines.toList)

  def matricesFromResource[M<:Nat:ToInt,N<:Nat:ToInt](resource: String):
    Dimension[M,Matrix[N,N,Double]] = {
      val num = implicitly[ToInt[M]].apply
      val dim = implicitly[ToInt[N]].apply
      val matrices = fromResource(s"${prefix}/$resource")
        .getLines
        .grouped(dim)
        .toList
        .map(lines => matrixFromLines[N](lines.toList).t)

      Sized.wrap(matrices.toVector.take(num))
    }

  def matrixFromResourceF[N<:Nat:ToInt](resource: String, f: List[String] => List[String]) =
    (f andThen matrixFromLines[N])(fromResource(s"${prefix}/$resource").getLines.toList)

  def matrixFromResourceTail[N<:Nat:ToInt](resource: String) =
    matrixFromResourceF(resource, _.tail)

  def matrix10FromResource[N<:Nat:ToInt](resource: String): Dimension10[Matrix[N,N,Double]] = {
    val dim = implicitly[ToInt[N]].apply
    val lines = fromResource(s"${prefix}/$resource")
      .getLines.toList

    val matrices = lines
      .grouped(dim)
      .map { group =>
        val elements = group
          .mkString
          .trim
          .split("\\s+")
          .map { _.toDouble }
          .grouped(dim)
          .map(_.toVector)
          .toVector
        Matrix.wrap[N,N,Double](elements: _*)
      }
    Sized.wrap(matrices.toVector)
  }

}
