package com.victormeyer.scalaml.model.algorithm

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec

class BinaryLogisticRegressionModelTest extends AnyFlatSpec {

  "BinaryLogisticRegressionModel.toString" should "format model in json" in {

    // given

    val weights: DenseMatrix[Double] = DenseMatrix.zeros(1, 5)
    val model: BinaryLogisticRegressionModel = new BinaryLogisticRegressionModel(weights.toArray, 0.9817)

    // when

    val modelString: String = model.toString

    // then

    assert(modelString == """{"weights": [0.0, 0.0, 0.0, 0.0, 0.0], "bias": 0.9817}""")

  }

}
