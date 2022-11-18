package com.victormeyer.scalaml.algorithm

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec

class ActivationsTest extends AnyFlatSpec {

  "Activations.sigmoid" should "compute sigmoid on matrix" in {

    // given

    val matrix: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0)

    // when

    val sigmoid: DenseMatrix[Double] = Activations.sigmoid(matrix)

    // then

    assert(sigmoid == DenseMatrix(0.7310585786300049, 0.8807970779778823, 0.9525741268224334).t)

  }

}
