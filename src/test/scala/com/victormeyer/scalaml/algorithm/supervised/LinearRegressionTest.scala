package com.victormeyer.scalaml.algorithm.supervised

import breeze.linalg.DenseMatrix
import com.victormeyer.scalaml.model.algorithm.LinearRegressionModel
import org.scalatest.flatspec.AnyFlatSpec

class LinearRegressionTest extends AnyFlatSpec {

  "LinearRegression.fit" should "compute linear regression with least square error on simple xTrain" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0)
    val yTrain: DenseMatrix[Double] = DenseMatrix(2.0, 4.0, 6.0)
    val linearRegression: LinearRegression = new LinearRegression
    linearRegression.setData(xTrain, yTrain)

    // when

    linearRegression.fit()

    // then

    assert(scala.math.pow(linearRegression.model.asInstanceOf[LinearRegressionModel].alpha(0) - 2.0, 2.0) < 0.0000001)

  }

  it should "compute linear regression with least square error on multidimensional xTrain" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((1.1, 2.0, 3.0), (2.0, 1.2, 3.0))
    val yTrain: DenseMatrix[Double] = DenseMatrix(3.0, 2.0)
    val linearRegression: LinearRegression = new LinearRegression
    linearRegression.setData(xTrain, yTrain)

    // when

    linearRegression.fit()

    // then

    assert(linearRegression.model.asInstanceOf[LinearRegressionModel].alpha.length == 3)

  }

  "LinearRegression.predict" should "apply model on simple xTest" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0)
    val yTrain: DenseMatrix[Double] = DenseMatrix(2.0, 4.0, 6.0)
    val xTest: DenseMatrix[Double] = DenseMatrix(3.0, 6.0, 9.0)
    val linearRegression: LinearRegression = new LinearRegression
    linearRegression.setData(xTrain, yTrain)
    linearRegression.fit()

    // when

    val yTest: DenseMatrix[Double] = linearRegression.predict(xTest)

    // then

    for(i <- 0 until yTest.cols){
      assert(scala.math.pow(xTest(0, i) * 2.0 - yTest(0, i), 2.0) < 0.0001)
    }

  }

  it should "apply model on multidimensional xTest" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((1.0, 8.0), (1.0, 11.0))
    val yTrain: DenseMatrix[Double] = DenseMatrix(3.0, 2.0)
    val xTest: DenseMatrix[Double] = DenseMatrix((2.0, 5.0), (1.0, 12.0))
    val linearRegression: LinearRegression = new LinearRegression
    linearRegression.setData(xTrain, yTrain)
    linearRegression.fit()

    // when

    val yTest: DenseMatrix[Double] = linearRegression.predict(xTest)

    // then

    assert(scala.math.pow(yTest(0, 0) - 9.66666, 2) < 0.000001)
    assert(scala.math.pow(yTest(1, 0) - 1.66666, 2) < 0.000001)

  }

}
