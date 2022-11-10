package com.victormeyer.scalaml.algorithm.supervised

import com.victormeyer.scalaml.data.Matrix
import com.victormeyer.scalaml.model.algorithm.SimpleLinearRegressionModel
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import scala.io.{BufferedSource, Source}

class SimpleLinearRegressionTest extends AnyFlatSpec {

  "SimpleLinearRegression.fit" should "process linear regression with least squares" in {

    // given

    val xTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val yTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val simpleLinearRegression: SimpleLinearRegression[Double] = new SimpleLinearRegression
    simpleLinearRegression.setData(xTrain, yTrain)

    // when

    simpleLinearRegression.fit

    // then

    assert(simpleLinearRegression.model.toString == new SimpleLinearRegressionModel(1.0, 0.0).toString)

  }

  it should "raise exception when matrices have invalid shape" in {

    // given

    val xTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4), Vector(1, 2, 3, 4)))
    val yTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val simpleLinearRegressionA: SimpleLinearRegression[Double] = new SimpleLinearRegression
    val simpleLinearRegressionB: SimpleLinearRegression[Double] = new SimpleLinearRegression
    simpleLinearRegressionA.setData(xTrain, yTrain)
    simpleLinearRegressionB.setData(yTrain, xTrain)
    var errorMessageA: String = ""
    var errorMessageB: String = ""

    // when

    try {
      simpleLinearRegressionA.fit
    } catch {
      case e: IllegalArgumentException => errorMessageA = e.getMessage
    }

    try {
      simpleLinearRegressionB.fit
    } catch {
      case e: IllegalArgumentException => errorMessageB = e.getMessage
    }

    // then

    assert(errorMessageA == "Invalid shapes: x or y hasn't only one row (xTrain: 2, yTrain: 1)")
    assert(errorMessageB == "Invalid shapes: x or y hasn't only one row (xTrain: 1, yTrain: 2)")

  }

  "SimpleLinearRegression.predict" should "apply model on matrix" in {

    // given

    val xTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val yTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val xTest: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4, 5, 6, 7, 8)))
    val simpleLinearRegression: SimpleLinearRegression[Double] = new SimpleLinearRegression
    simpleLinearRegression.setData(xTrain, yTrain)
    simpleLinearRegression.fit

    // when

    val yTest: Matrix[Double] = simpleLinearRegression.predict(xTest)

    // then

    assert(yTest.equals(xTest))

  }

  "SimpleLinearRegression.saveModel" should "create file with json model" in {

    // given

    val xTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val yTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val simpleLinearRegression: SimpleLinearRegression[Double] = new SimpleLinearRegression
    simpleLinearRegression.setData(xTrain, yTrain)
    simpleLinearRegression.fit

    // when

    simpleLinearRegression.saveModel("src/test/resources/model", "MySimpleLinearModel")
    val outputFilePath: String = new File("src/test/resources/model").listFiles.filter(_.getName.contains("MySimpleLinearModel"))(0).getAbsolutePath
    val outputFile: BufferedSource = Source.fromFile(outputFilePath)
    val fileContent: String = outputFile.mkString
    outputFile.close()

    // then

    assert(fileContent.contains("""{
                                  |  "modelType": "SimpleLinearRegression",
                                  |  "modelName": "MySimpleLinearModel",""".stripMargin))
    assert(fileContent.contains(""""dateMaj""""))
    assert(fileContent.contains("""  "model": {
                                  |    "alpha": 1.0,
                                  |    "beta": 0.0
                                  |  }
                                  |}""".stripMargin))

    // after

    new File(outputFilePath).delete()

  }

  "SimpleLinearRegression.loadModel" should "get model from file" in {

    // given

    val xTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val yTrain: Matrix[Double] = Matrix.vectorToMatrix(Vector(Vector(1, 2, 3, 4)))
    val simpleLinearRegressionA: SimpleLinearRegression[Double] = new SimpleLinearRegression
    val simpleLinearRegressionB: SimpleLinearRegression[Double] = new SimpleLinearRegression
    simpleLinearRegressionA.setData(xTrain, yTrain)
    simpleLinearRegressionA.fit
    simpleLinearRegressionA.saveModel("src/test/resources/model", "MySimpleLinearModelToLoad")
    val outputFilePath: String = new File("src/test/resources/model").listFiles.filter(_.getName.contains("MySimpleLinearModelToLoad"))(0).getAbsolutePath

    // when

    simpleLinearRegressionB.loadModel(outputFilePath)

    // then

    assert(simpleLinearRegressionB.model.asInstanceOf[SimpleLinearRegressionModel].alpha ==  1.0)
    assert(simpleLinearRegressionB.model.asInstanceOf[SimpleLinearRegressionModel].beta == 0.0)

    // after

    new File(outputFilePath).delete()

  }

}
