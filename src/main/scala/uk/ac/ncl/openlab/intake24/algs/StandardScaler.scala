package uk.ac.ncl.openlab.intake24.algs

/**
  * Created by Tim Osadchiy on 30/04/2018.
  */
case class StandardScaler(withMean: Boolean = true, withStd: Boolean = true) {

  def fit(matrix: Vector[Vector[Double]]) = {
    StandardScaler.validateMatrix(matrix)
    ScalerModel(withMean, withStd, StandardScaler.mean(matrix), StandardScaler.std(matrix), matrix.head.size)
  }

}

/**
  * Created by Tim Osadchiy on 27/04/2018.
  */

object StandardScaler {

  def findInconsistency(matrix: Vector[Vector[Double]]): Option[Int] = {
    val size = matrix.head.size
    matrix.zipWithIndex.find(v => v._1.size != size).map(v => v._2)
  }

  def validateMatrix(matrix: Vector[Vector[Double]]) = {
    if (matrix.isEmpty) {
      throw new IllegalArgumentException("Matrix is empty")
    }

    val inc = StandardScaler.findInconsistency(matrix)
    if (inc.isDefined) {
      throw new IllegalArgumentException(s"Matrix shape is not consistent at ${inc.getOrElse(-1)}")
    }
  }

  def validateTransformedMatrix(matrix: Vector[Vector[Double]], size: Int) = {
    if (matrix.isEmpty) {
      throw new IllegalArgumentException("Matrix is empty")
    }

    val inc = matrix.find(v => v.size != size)
    if (inc.isDefined) {
      throw new IllegalArgumentException(s"Vector at index ${inc.getOrElse(-1)} is of unexpected size. Expected size is $size")
    }
  }

  def transpose(matrix: Vector[Vector[Double]]): Vector[Vector[Double]] = {
    matrix.head.indices.map(i => matrix.map(_ (i))).toVector
  }

  def mean(matrix: Vector[Vector[Double]]): Vector[Double] = {
    val tr = transpose(matrix)
    tr.map(v => v.sum / v.size)
  }

  def std(matrix: Vector[Vector[Double]]) = {
    val tr = transpose(matrix)
    val mn = mean(matrix)
    tr.zipWithIndex.map { vi =>
      Math.sqrt(vi._1.map(x => Math.pow(x - mn(vi._2), 2)).sum / vi._1.size)
    }
  }

}

case class ScalerModel(withMean: Boolean, withStd: Boolean, mean: Vector[Double], std: Vector[Double], vectorSize: Int) {

  def transform(matrix: Vector[Vector[Double]]) = {
    StandardScaler.validateTransformedMatrix(matrix, vectorSize)
    matrix.map { vector =>
      vector.zipWithIndex.map { xi =>
        val mn = if (withMean) mean(xi._2) else 0d
        val s = if (withStd) std(xi._2) else 1d
        (xi._1 - mn) / s
      }
    }
  }

}