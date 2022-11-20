package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix

import java.lang.reflect.{Field, Method}
import java.nio.file.Paths
import java.util.Calendar
import scala.collection.mutable

/** Trait which generate report for classification.
 * Classes who implemented this method should have variables xTrain, yTrain, xTest, yTest, classes and reportDirectory.
 *
 */
trait ReportClassification extends Report {

  val xTrainField: Field = this.getClass.getDeclaredField("xTrain")
  xTrainField.setAccessible(true)
  val yTrainField: Field = this.getClass.getDeclaredField("yTrain")
  yTrainField.setAccessible(true)
  val xTestField: Field = this.getClass.getDeclaredField("xTest")
  xTestField.setAccessible(true)
  val yTestField: Field = this.getClass.getDeclaredField("yTest")
  yTestField.setAccessible(true)
  val classesField: Field = this.getClass.getDeclaredField("classes")
  classesField.setAccessible(true)
  val reportDirectoryField: Field = this.getClass.getDeclaredField("reportDirectory")
  reportDirectoryField.setAccessible(true)

  val predictMethod: Method = this.getClass.getMethod("predict", classOf[DenseMatrix[Double]])
  predictMethod.setAccessible(true)

  val trainAccuracies: mutable.ArrayBuffer[Double] = new mutable.ArrayBuffer[Double]()
  val trainLosses: mutable.ArrayBuffer[Double] = new mutable.ArrayBuffer[Double]()
  val testAccuracies: mutable.ArrayBuffer[Double] = new mutable.ArrayBuffer[Double]()
  val testLosses: mutable.ArrayBuffer[Double] = new mutable.ArrayBuffer[Double]()

  val startTime: Long = Calendar.getInstance.getTimeInMillis
  val reportMainName: String = this.getClass.getSimpleName + "-" + startTime

  /** Implements this method to add particular metadata to the resume
   *
   * @return Map of metadata, with key as metadata name and value as metadata value
   */
  def metadata: Map[String, String]

  /** Call this method to generate the classification report
   *
   * @param epochs Tuple with current epochs and total epochs
   * @param lossType Type of loss function
   * @param accuracyType Type of accuracy function
   */
  def report(epochs: (Int, Int), lossType: String, accuracyType: String): Unit ={
    initReport(Paths.get(reportDirectoryField.get(this).asInstanceOf[String], reportMainName, epochs._1.toString).toString)
    addTable("Resume", generateResume(epochs, lossType, accuracyType))
    addTrainResult(epochs, lossType, accuracyType)
    if(testAccuracies.nonEmpty && testLosses.nonEmpty){
      addTestResult(epochs, lossType, accuracyType)
    }
    generate("report")
  }

  /** Add train report to the final report
   *
   * @param epochs Tuple with current epochs and total epochs
   * @param lossType Type of loss function
   * @param accuracyType Type of accuracy function
   */
  private def addTrainResult(epochs: (Int, Int), lossType: String, accuracyType: String): Unit ={
    val epochsArray: Array[Double] = Array.range(0, epochs._1).map(_.toDouble)
    val expected: DenseMatrix[Double] = yTrainField.get(this).asInstanceOf[DenseMatrix[Double]]
    val predictions: DenseMatrix[Double] =
      predictMethod.invoke(this, xTrainField.get(this).asInstanceOf[DenseMatrix[Double]]).asInstanceOf[DenseMatrix[Double]]
    add2dGraphic("Train Accuracy", epochsArray, trainAccuracies.toArray, "epochs", accuracyType)
    add2dGraphic("Train Loss", epochsArray, trainLosses.toArray, "epochs", lossType)
    addTable("Train Expected / Predictions", generatePredictionMatrix(predictions.toArray, expected.toArray))
  }

  /** Add test report to the final report
   *
   * @param epochs Tuple with current epochs and total epochs
   * @param lossType Type of loss function
   * @param accuracyType Type of accuracy function
   */
  private def addTestResult(epochs: (Int, Int), lossType: String, accuracyType: String): Unit ={
    val epochsArray: Array[Double] = Array.range(0, epochs._1).map(_.toDouble)
    val expected: DenseMatrix[Double] = yTestField.get(this).asInstanceOf[DenseMatrix[Double]]
    val predictions: DenseMatrix[Double] =
      predictMethod.invoke(this, xTestField.get(this).asInstanceOf[DenseMatrix[Double]]).asInstanceOf[DenseMatrix[Double]]
    add2dGraphic("Test Accuracy", epochsArray, testAccuracies.toArray, "epochs", accuracyType)
    add2dGraphic("Test Loss", epochsArray, testLosses.toArray, "epochs", lossType)
    addTable("Test Expected / Predictions", generatePredictionMatrix(predictions.toArray, expected.toArray))
  }

  /** Generate the matrix gathering metadata
   *
   * @param epochs Tuple with current epochs and total epochs
   * @param lossType Type of loss function
   * @param accuracyType Type of accuracy function
   * @return DenseMatrix of string with two column: metadata type and value
   */
  private def generateResume(epochs: (Int, Int), lossType: String, accuracyType: String): DenseMatrix[String] ={
    val rows: mutable.ArrayBuffer[Array[String]] = mutable.ArrayBuffer[Array[String]]()
    rows += Array("Algorithm", this.getClass.getSimpleName)
    rows += Array("Duration (ms)", (Calendar.getInstance.getTimeInMillis - startTime).toString)
    rows += Array("Epochs", s"${epochs._1} / ${epochs._2}")
    for((k, v) <- getShape)
      rows += Array(k, v)
    for((k, v) <- metadata)
      rows += Array(k, v)
    rows += Array("Accuracy Type", accuracyType)
    rows += Array("Loss Type", lossType)
    rows += Array("Train Accuracy", trainAccuracies.last.toString)
    rows += Array("Train Loss", trainLosses.last.toString)
    if(testLosses.nonEmpty && testAccuracies.nonEmpty){
      rows += Array("Test Accuracy", testAccuracies.last.toString)
      rows += Array("Test Loss", testLosses.last.toString)
    }
    DenseMatrix(rows.toArray:_*)
  }

  /** Return shape of all fit matrices
   *
   * @return Map with key as matrix name and value as shape
   */
  private def getShape: Map[String, String] ={
    val xTrain: DenseMatrix[Double] = xTrainField.get(this).asInstanceOf[DenseMatrix[Double]]
    val yTrain: DenseMatrix[Double] = yTrainField.get(this).asInstanceOf[DenseMatrix[Double]]
    val xTest: DenseMatrix[Double] = xTestField.get(this).asInstanceOf[DenseMatrix[Double]]
    val yTest: DenseMatrix[Double] = yTestField.get(this).asInstanceOf[DenseMatrix[Double]]
    val shapeMap: Map[String, String] = Map("xTrain" -> (xTrain.rows, xTrain.cols).toString, "yTrain" -> (yTrain.rows, yTrain.cols).toString)
    if(xTest != null && yTest != null){
      shapeMap ++ Map("xTest" -> (xTest.rows, xTest.cols).toString, "yTest" -> (yTest.rows, yTest.cols).toString)
    } else {
      shapeMap
    }
  }

  /** Create expected / predictions matrix for each class
   *
   * @param predictions Array of predictions
   * @param expected Array of expected value
   * @return DenseMatrix with all classes and their expected count and their effective count
   */
  private def generatePredictionMatrix(predictions: Array[Double], expected: Array[Double]): DenseMatrix[String] ={
    val classes: Map[Double, String] = classesField.get(this).asInstanceOf[Map[Double, String]]
    val rows: mutable.ArrayBuffer[Array[String]] = mutable.ArrayBuffer[Array[String]]()
    val classKeys: Array[Double] = classes.keys.toArray
    val resultWithExpected: Array[(Double, Double)] = expected.zip(predictions)
    val expectedCountByClass: Array[String] = classKeys.map(c => expected.count(_ == c).toString)
    rows += Array.concat(Array("labels"), classes.values.toArray)
    rows += Array.concat(Array("expected"), expectedCountByClass)
    classKeys.foreach(c => {
      val classPredictions: Array[Double] = resultWithExpected.filter(_._1 == c).map(_._2)
      val predictionsGroupCount: Array[String] = classKeys.map(c => classPredictions.count(_ == c).toString)
      rows += Array.concat(Array(s"${classes(c)} predict"), predictionsGroupCount)
    })
    DenseMatrix(rows.toArray:_*)
  }

}
