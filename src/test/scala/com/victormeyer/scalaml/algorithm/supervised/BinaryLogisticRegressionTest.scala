package com.victormeyer.scalaml.algorithm.supervised

import breeze.linalg.DenseMatrix
import com.victormeyer.scalaml.model.algorithm.BinaryLogisticRegressionModel
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.lang.reflect.Field
import scala.collection.mutable
import scala.reflect.io.Directory

class BinaryLogisticRegressionTest extends AnyFlatSpec with PrivateMethodTester {

  "BinaryLogisticRegression.fit" should "train model with test data" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (0.0, 0.2), (0.2, 0.1), (1.0, 1.1), (1.0, 1.2), (1.2, 1.1))
    val yTrain: DenseMatrix[Double] = DenseMatrix(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val classes: Map[Double, String] = Map(0.0 -> "cat", 1.0 -> "dog")
    val logisticRegression: BinaryLogisticRegression =
      new BinaryLogisticRegression(xTrain, yTrain, xTrain, yTrain, classes, 1000, 0.1, reportDirectory="src/test/resources/report/blr0")
    val trainAccuraciesField: Field = logisticRegression.getClass.getDeclaredField("trainAccuracies")
    trainAccuraciesField.setAccessible(true)
    val trainLossesField: Field = logisticRegression.getClass.getDeclaredField("trainLosses")
    trainLossesField.setAccessible(true)
    val testAccuraciesField: Field = logisticRegression.getClass.getDeclaredField("testAccuracies")
    testAccuraciesField.setAccessible(true)
    val testLossesField: Field = logisticRegression.getClass.getDeclaredField("testLosses")
    testLossesField.setAccessible(true)

    // when

    logisticRegression.fit()
    val model: BinaryLogisticRegressionModel = logisticRegression.model.asInstanceOf[BinaryLogisticRegressionModel]
    val trainAccuracies: mutable.ArrayBuffer[Double] = trainAccuraciesField.get(logisticRegression).asInstanceOf[mutable.ArrayBuffer[Double]]
    val trainLosses: mutable.ArrayBuffer[Double] = trainLossesField.get(logisticRegression).asInstanceOf[mutable.ArrayBuffer[Double]]
    val testAccuracies: mutable.ArrayBuffer[Double] = testAccuraciesField.get(logisticRegression).asInstanceOf[mutable.ArrayBuffer[Double]]
    val testLosses: mutable.ArrayBuffer[Double] = testLossesField.get(logisticRegression).asInstanceOf[mutable.ArrayBuffer[Double]]

    // then

    assert(4.7 < model.weights(0) && model.weights(0) < 4.8)
    assert(3.7 < model.weights(1) && model.weights(1) < 3.8)
    assert(-4.3 < model.bias && model.bias < -4.2)
    trainAccuracies.drop(1).zip(testAccuracies.drop(1)).foreach(pair => assert(scala.math.pow(pair._1 - pair._2, 2.0) < 1e-9))
    trainLosses.zip(testLosses).foreach(pair => assert(scala.math.pow(pair._1 - pair._2, 2.0) < 1e-9))

    // after

    new Directory(new File("src/test/resources/report/blr0")).deleteRecursively()

  }

  it should "train model without test data" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (0.0, 0.2), (0.2, 0.1), (1.0, 1.1), (1.0, 1.2), (1.2, 1.1))
    val yTrain: DenseMatrix[Double] = DenseMatrix(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val classes: Map[Double, String] = Map(0.0 -> "cat", 1.0 -> "dog")
    val logisticRegression: BinaryLogisticRegression =
      new BinaryLogisticRegression(xTrain=xTrain, yTrain=yTrain, classes=classes, reportDirectory="src/test/resources/report/blr1")
    val testAccuraciesField: Field = logisticRegression.getClass.getDeclaredField("testAccuracies")
    testAccuraciesField.setAccessible(true)
    val testLossesField: Field = logisticRegression.getClass.getDeclaredField("testLosses")
    testLossesField.setAccessible(true)

    // when

    logisticRegression.fit()
    val model: BinaryLogisticRegressionModel = logisticRegression.model.asInstanceOf[BinaryLogisticRegressionModel]
    val testAccuracies: mutable.ArrayBuffer[Double] = testAccuraciesField.get(logisticRegression).asInstanceOf[mutable.ArrayBuffer[Double]]
    val testLosses: mutable.ArrayBuffer[Double] = testLossesField.get(logisticRegression).asInstanceOf[mutable.ArrayBuffer[Double]]

    // then

    assert(4.7 < model.weights(0) && model.weights(0) < 4.8)
    assert(3.7 < model.weights(1) && model.weights(1) < 3.8)
    assert(-4.3 < model.bias && model.bias < -4.2)
    assert(testAccuracies.length < 1)
    assert(testLosses.length < 1)

    // after

    new Directory(new File("src/test/resources/report/blr1")).deleteRecursively()

  }

  it should "generate multiple report when reportFrequency is define" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (0.0, 0.2), (0.2, 0.1), (1.0, 1.1), (1.0, 1.2), (1.2, 1.1))
    val yTrain: DenseMatrix[Double] = DenseMatrix(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val classes: Map[Double, String] = Map(0.0 -> "cat", 1.0 -> "dog")
    val logisticRegression: BinaryLogisticRegression =
      new BinaryLogisticRegression(xTrain=xTrain, yTrain=yTrain, classes=classes, reportFrequency=100, reportDirectory="src/test/resources/report/blr2")

    // when

    logisticRegression.fit()
    val reportFolderPath: String = new File("src/test/resources/report/blr2").listFiles()(0).getAbsolutePath

    // then

    assert(new File(reportFolderPath).listFiles().length == 10)

    // after

    new Directory(new File("src/test/resources/report/blr2")).deleteRecursively()

  }

  "BinaryLogisticRegression.predict" should "apply model and sigmoid on matrix" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (0.0, 0.2), (0.2, 0.1), (1.0, 1.1), (1.0, 1.2), (1.2, 1.1))
    val yTrain: DenseMatrix[Double] = DenseMatrix(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)
    val classes: Map[Double, String] = Map(0.0 -> "cat", 1.0 -> "dog")
    val logisticRegression: BinaryLogisticRegression =
      new BinaryLogisticRegression(xTrain, yTrain, xTrain, yTrain, classes, 1000, 0.1, reportDirectory = "src/test/resources/report/blr3")
    logisticRegression.fit()

    // when

    val predictions: DenseMatrix[Double] = logisticRegression.predict(DenseMatrix((0.0, 0.0), (0.3, 0.2), (1.0, 1.0), (1.1, 1.2)))

    // then

    assert(predictions == DenseMatrix(0.0, 0.0, 1.0, 1.0))

    // after

    new Directory(new File("src/test/resources/report/blr3")).deleteRecursively()

  }

  "BinaryLogisticRegression.applyModel" should "apply model on matrix" in {

    // given

    val xTrain: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (0.0, 0.2), (0.2, 0.1), (1.0, 1.1), (1.0, 1.2), (1.2, 1.1))
    val weights: DenseMatrix[Double] = DenseMatrix.zeros(1, xTrain.cols)
    val alpha: Double = 0.0
    val logisticRegression: BinaryLogisticRegression = new BinaryLogisticRegression()
    val applyModel: PrivateMethod[DenseMatrix[Double]] = PrivateMethod[DenseMatrix[Double]]('applyModel)

    // when

    val ingestedData: DenseMatrix[Double] =
      logisticRegression invokePrivate applyModel(weights, alpha, xTrain)

    // then

    assert(ingestedData == DenseMatrix(0.5, 0.5, 0.5, 0.5, 0.5, 0.5))

  }

  "BinaryLogisticRegression.gradientDescent" should "return derivative of loss function" in {

    // given

    val logisticRegression: BinaryLogisticRegression = new BinaryLogisticRegression()
    val gradientDescent: PrivateMethod[(DenseMatrix[Double], Double)] = PrivateMethod[(DenseMatrix[Double], Double)]('gradientDescent)
    val x: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (0.0, 0.2), (0.2, 0.1), (1.0, 1.1), (1.0, 1.2), (1.2, 1.1))
    val y: DenseMatrix[Double] = DenseMatrix(0.0, 0.0, 0.0, 1.0, 1.0, 1.0)

    // when

    val (weights, bias): (DenseMatrix[Double], Double) = logisticRegression invokePrivate gradientDescent(x, y, y)

    // then

    assert(weights == DenseMatrix(0.0, 0.0).t)
    assert(bias == 0.0)

  }

  "BinaryLogisticRegression.matrixToBinary" should "convert each value to 0.0 or 1.0" in {

    // given

    val logisticRegression: BinaryLogisticRegression = new BinaryLogisticRegression()
    val matrixToBinary: PrivateMethod[DenseMatrix[Double]] = PrivateMethod[DenseMatrix[Double]]('matrixToBinary)
    val matrix: DenseMatrix[Double] = DenseMatrix((0.0, 0.1), (1.2, 1.1))

    // when

    val binaryMatrix: DenseMatrix[Double] = logisticRegression invokePrivate matrixToBinary(matrix)

    // then

    assert(binaryMatrix == DenseMatrix((0.0, 0.0), (1.0, 1.0)))

  }

  "BinaryLogisticRegression.metadata" should "return map with alpha" in {

    // given

    val logisticRegression: BinaryLogisticRegression = new BinaryLogisticRegression(alpha= 1.23)

    // when

    val metadata: Map[String, String] = logisticRegression.metadata

    // then

    assert(metadata == Map("alpha" -> "1.23"))

  }

}
