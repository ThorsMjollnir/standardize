package uk.ac.ncl.openlab.intake24.algs

import org.scalactic.Equality
import org.scalatest.FunSuite

/**
  * Created by Tim Osadchiy on 27/04/2018.
  */
class StandardScaleTest extends FunSuite {

  val inMatrix = Vector(
    Vector(2d, 100d),
    Vector(10d, 200d),
    Vector(4d, 120d),
    Vector(6d, 130d)
  )

  implicit val doubleEquality: Equality[Double] =
    (a: Double, b: Any) => Math.abs(a - b.asInstanceOf[Double]) < 1e-8

  def assertAnswer(expected: Vector[Vector[Double]], actual: Vector[Vector[Double]]) =
    expected.zipWithIndex.forall { ev =>
      ev._1.zipWithIndex.forall(sev => actual(ev._2)(sev._2) === sev._1)
    }

  test("Mean is calculated correctly") {

    val answer = Vector(5.5, 137.5)
    val mean = StandardScaler.mean(inMatrix)
    assert(mean.zipWithIndex.forall(i => i._1 === answer(i._2)))

  }

  test("Std is calculated correctly") {

    val answer = Vector(2.95803989, 37.66629793)
    val std = StandardScaler.std(inMatrix)
    assert(std.zipWithIndex.forall(i => i._1 === answer(i._2)))

  }

  test("Transform with std only") {

    val answer = Vector(
      Vector(0.6761234, 2.65489325),
      Vector(3.38061702, 5.30978649),
      Vector(1.35224681, 3.1858719),
      Vector(2.02837021, 3.45136122)
    )

    val model = StandardScaler(withMean = false).fit(inMatrix)
    val transformed = model.transform(inMatrix)
    assert(assertAnswer(answer, transformed))

  }

  test("Transform with mean only") {

    val answer = Vector(
      Vector(-3.5, -37.5),
      Vector(4.5, 62.5),
      Vector(-1.5, -17.5),
      Vector(0.5, -7.5)
    )

    val model = StandardScaler(withStd = false).fit(inMatrix)
    val transformed = model.transform(inMatrix)
    assert(assertAnswer(answer, transformed))

  }

  test("Transform with std and mean") {

    val answer = Vector(
      Vector(-1.18321596, -0.99558497),
      Vector(1.52127766, 1.65930828),
      Vector(-0.50709255, -0.46460632),
      Vector(0.16903085, -0.19911699)
    )

    val model = StandardScaler().fit(inMatrix)
    val transformed = model.transform(inMatrix)
    assert(assertAnswer(answer, transformed))

  }

}
