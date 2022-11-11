package com.victormeyer.scalaml.model.algorithm

import org.scalatest.flatspec.AnyFlatSpec

class LinearRegressionModelTest extends AnyFlatSpec {

  "LinearRegressionModel.toString" should "format model in json" in {

    // given

    val model: LinearRegressionModel = new LinearRegressionModel(Array(1.0, 2.0, 3.0))

    // when

    val modelString: String = model.toString

    // then

    assert(modelString == """{"alpha": [1.0, 2.0, 3.0]}""")

  }

}
