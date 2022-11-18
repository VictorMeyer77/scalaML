package com.victormeyer.scalaml.algorithm

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec

class LossesTest extends AnyFlatSpec {

  "Losses.binaryCrossEntropy" should "compute binary cross entropy" in {

    // given

    val expected: DenseMatrix[Double] = DenseMatrix(0.0, 1.0, 0.0, 1.0)
    val worstPredictions: DenseMatrix[Double] = DenseMatrix(1.0, 0.0, 1.0, 0.0)

    // when

    val bestBCE: Double = Losses.binaryCrossEntropy(expected, expected)
    val worstBCE: Double = Losses.binaryCrossEntropy(expected, worstPredictions)

    // then

    assert(bestBCE < 1e-9)
    assert(worstBCE == 20.72326583694641)

  }

}
