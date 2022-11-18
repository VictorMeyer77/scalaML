package com.victormeyer.scalaml.algorithm

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec

class AccuraciesTest extends AnyFlatSpec {

  "Accuracies.f1Score" should "compute f1score correctly" in {

    // given

    val actual: DenseMatrix[Double] = DenseMatrix(1.0, 0.0, 1.0, 0.0)
    val worstPredictions: DenseMatrix[Double] = DenseMatrix(0.0, 1.0, 0.0, 1.0)

    // when

    val bestF1Score: Double = Accuracies.f1Score(actual, actual)
    val worstF1Score: Double = Accuracies.f1Score(actual, worstPredictions)

    // then

    assert(bestF1Score == 1.0)
    assert(worstF1Score == 0.0)

  }

  it should "raise exception when matrices have not same number of rows" in {

    // given

    val actual: DenseMatrix[Double] = DenseMatrix(1.0, 0.0, 1.0, 0.0)
    val worstPredictions: DenseMatrix[Double] = DenseMatrix(0.0, 1.0, 0.0, 1.0, 1.0)
    var errorMessage: String = ""

    // when

    try {
      Accuracies.f1Score(actual, worstPredictions)
    } catch {
      case e: IllegalArgumentException => errorMessage = e.getMessage
    }

    // then

    assert(errorMessage == "expected and predictions shapes are not equal: 4 != 5")

  }

}
