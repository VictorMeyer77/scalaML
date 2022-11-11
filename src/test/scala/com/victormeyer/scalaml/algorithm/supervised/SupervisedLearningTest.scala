package com.victormeyer.scalaml.algorithm.supervised

import breeze.linalg.DenseMatrix
import com.victormeyer.scalaml.model.algorithm.LinearRegressionModel
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File

class SupervisedLearningTest extends AnyFlatSpec {

  "SupervisedLearning.setData" should "hydrate xTrain and yTrain" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0)
    val yTrain: DenseMatrix[Double] = DenseMatrix(2.0, 4.0, 6.0)
    val linearRegression: LinearRegression = new LinearRegression

    // when

    linearRegression.setData(xTrain, yTrain)

    // then

    assert(linearRegression.xTrain == xTrain)
    assert(linearRegression.yTrain == yTrain)

  }

  "SupervisedLearning.saveModel" should "save model in json file" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0)
    val yTrain: DenseMatrix[Double] = DenseMatrix(2.0, 4.0, 6.0)
    val linearRegression: LinearRegression = new LinearRegression
    linearRegression.setData(xTrain, yTrain)
    linearRegression.fit()

    // when

    linearRegression.saveModel("src/test/resources/model", "MyFooModel")

    // then

    assert(new File("src/test/resources/model").listFiles.count(_.getName.contains("MyFooModel")) == 1)

    // after

    new File(new File("src/test/resources/model").listFiles.filter(_.getName.contains("MyFooModel"))(0).getAbsolutePath).delete()

  }

  "SupervisedLearning.loadModel" should "load model from json file" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix(1.0, 2.0, 3.0)
    val yTrain: DenseMatrix[Double] = DenseMatrix(2.0, 4.0, 6.0)
    val linearRegression: LinearRegression = new LinearRegression
    linearRegression.setData(xTrain, yTrain)
    linearRegression.fit()
    linearRegression.saveModel("src/test/resources/model", "MyFooModelToLoad")
    val modelPath: String = new File("src/test/resources/model").listFiles.filter(_.getName.contains("MyFooModelToLoad"))(0).getAbsolutePath

    // when

    linearRegression.loadModel(modelPath)

    // then

    assert(scala.math.pow(linearRegression.model.asInstanceOf[LinearRegressionModel].alpha(0) - 2.0, 2.0) < 0.0000001)

    // after

    new File(modelPath).delete()

  }

}
