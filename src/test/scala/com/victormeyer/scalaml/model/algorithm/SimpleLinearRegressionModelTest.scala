package com.victormeyer.scalaml.model.algorithm

import org.scalatest.flatspec.AnyFlatSpec

class SimpleLinearRegressionModelTest extends AnyFlatSpec {

  "SimpleLinearRegressionModel.toString" should "format SimpleLinearRegression to json" in {

    // given

    val simpleLinearRegressionModel: SimpleLinearRegressionModel = new SimpleLinearRegressionModel(0.24526, 819.9)

    // when

    val modelString: String = simpleLinearRegressionModel.toString

    // then

    assert(modelString == """{"alpha": 0.24526, "beta": 819.9}""")

  }

}
