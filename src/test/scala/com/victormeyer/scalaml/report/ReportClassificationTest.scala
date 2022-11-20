package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.nio.file.Paths
import scala.io.{BufferedSource, Source}
import scala.reflect.io.Directory

class ReportClassificationTester(val xTrain: DenseMatrix[Double],
                                 val yTrain: DenseMatrix[Double],
                                 val xTest: DenseMatrix[Double],
                                 val yTest: DenseMatrix[Double],
                                 val classes: Map[Double, String],
                                 val reportDirectory: String) extends ReportClassification {

  override def metadata: Map[String, String] ={
    Map("param" -> "18")
  }

  def fitWithTest(): Unit ={
    trainLosses += 1.0
    trainAccuracies += 0.1
    testLosses += 0.9
    testAccuracies += 0.8
  }

  def fitWithoutTest(): Unit ={
    trainLosses += 1.0
    trainAccuracies += 0.8
  }

  def predict(input: DenseMatrix[Double]): DenseMatrix[Double] ={
    DenseMatrix.zeros(input.rows, input.cols)
  }

}

class ReportClassificationTest extends AnyFlatSpec with PrivateMethodTester {

  "ReportClassification.report" should "generate report without test data" in {

    // given

    val testerWithoutTest: ReportClassificationTester = new ReportClassificationTester(
      DenseMatrix((0.0, 0.1), (1.2, 1.1)),
      DenseMatrix(0.0, 1.0),
      null,
      null,
      Map(0.0 -> "cat", 1.0 -> "dog"),
      "src/test/resources/report/ReportClassificationTest"
    )
    testerWithoutTest.fitWithoutTest()

    // when

    testerWithoutTest.report((100, 100), "Binary Cross Entropy", "F1-Score")
    val reportFolderPath: String = new File("src/test/resources/report/ReportClassificationTest").listFiles()(0).getAbsolutePath
    val trainAccuracyImgName: String =
      new File(Paths.get(reportFolderPath, "100/static/img").toString).listFiles().filter(file => file.getName.contains("Train Accuracy"))(0).getName
    val trainLossImgName: String =
      new File(Paths.get(reportFolderPath, "100/static/img").toString).listFiles().filter(file => file.getName.contains("Train Loss"))(0).getName
    val reportFile: BufferedSource = Source.fromFile(Paths.get(reportFolderPath, "100/report.html").toString)
    val reportContent: String = reportFile.mkString
    reportFile.close()

    // then

    assert(reportContent.contains("Duration (ms)"))
    assert(reportContent.split("\n").filter(row => !row.contains("Duration (ms)")).mkString("\n") ==
    s"""<!DOCTYPE html>
      |<html>
      |<head>
      |<title>report</title>
      |<style>
      |table, th, td {
      |  border: 1px solid;
      |}
      |</style>
      |</head>
      |<body>
      |<h1>report</h1>
      |
      |<div class="tableBlock">
      |	<table>
      |		<tr><th>Algorithm</th><th>ReportClassificationTester</th></tr>
      |		<tr><td>Epochs</td><td>100 / 100</td></tr>
      |		<tr><td>xTrain</td><td>(2,2)</td></tr>
      |		<tr><td>yTrain</td><td>(2,1)</td></tr>
      |		<tr><td>param</td><td>18</td></tr>
      |		<tr><td>Accuracy Type</td><td>F1-Score</td></tr>
      |		<tr><td>Loss Type</td><td>Binary Cross Entropy</td></tr>
      |		<tr><td>Train Accuracy</td><td>0.8</td></tr>
      |		<tr><td>Train Loss</td><td>1.0</td></tr>
      |	</table>
      |	<p>Resume</p>
      |</div><div class="imgBlock">
      |  <figure>
      |    <img src="static/img/$trainAccuracyImgName" alt="Train Accuracy" width="700" height="400" />
      |    <figcaption>Train Accuracy</figcaption>
      |  </figure>
      |</div><div class="imgBlock">
      |  <figure>
      |    <img src="static/img/$trainLossImgName" alt="Train Loss" width="700" height="400" />
      |    <figcaption>Train Loss</figcaption>
      |  </figure>
      |</div>
      |<div class="tableBlock">
      |	<table>
      |		<tr><th>labels</th><th>cat</th><th>dog</th></tr>
      |		<tr><td>expected</td><td>1</td><td>1</td></tr>
      |		<tr><td>cat predict</td><td>1</td><td>0</td></tr>
      |		<tr><td>dog predict</td><td>1</td><td>0</td></tr>
      |	</table>
      |	<p>Train Expected / Predictions</p>
      |</div>
      |</body>
      |</html>""".stripMargin)

    // after

    new Directory(new File("src/test/resources/report/ReportClassificationTest")).deleteRecursively()

  }

  it should "generate report with test data" in {

    // given

    val testerWithoutTest: ReportClassificationTester = new ReportClassificationTester(
      DenseMatrix((0.0, 0.1), (1.2, 1.1)),
      DenseMatrix(0.0, 1.0),
      DenseMatrix((1.0, 1.1, 0.0), (0.2, 0.1, 0.0)),
      DenseMatrix(1.0, 0.0, 1.0),
      Map(0.0 -> "cat", 1.0 -> "dog"),
      "src/test/resources/report/ReportClassificationTest2"
    )
    testerWithoutTest.fitWithTest()

    // when

    testerWithoutTest.report((100, 100), "Binary Cross Entropy", "F1-Score")
    val reportFolderPath: String = new File("src/test/resources/report/ReportClassificationTest2").listFiles()(0).getAbsolutePath
    val trainAccuracyImgName: String =
      new File(Paths.get(reportFolderPath, "100/static/img").toString).listFiles().filter(file => file.getName.contains("Train Accuracy"))(0).getName
    val trainLossImgName: String =
      new File(Paths.get(reportFolderPath, "100/static/img").toString).listFiles().filter(file => file.getName.contains("Train Loss"))(0).getName
    val testAccuracyImgName: String =
      new File(Paths.get(reportFolderPath, "100/static/img").toString).listFiles().filter(file => file.getName.contains("Test Accuracy"))(0).getName
    val testLossImgName: String =
      new File(Paths.get(reportFolderPath, "100/static/img").toString).listFiles().filter(file => file.getName.contains("Test Loss"))(0).getName
    val reportFile: BufferedSource = Source.fromFile(Paths.get(reportFolderPath, "100/report.html").toString)
    val reportContent: String = reportFile.mkString
    reportFile.close()

    // then

    assert(reportContent.contains("Duration (ms)"))
    assert(reportContent.split("\n").filter(row => !row.contains("Duration (ms)")).mkString("\n") ==
      s"""<!DOCTYPE html>
         |<html>
         |<head>
         |<title>report</title>
         |<style>
         |table, th, td {
         |  border: 1px solid;
         |}
         |</style>
         |</head>
         |<body>
         |<h1>report</h1>
         |
         |<div class="tableBlock">
         |	<table>
         |		<tr><th>Algorithm</th><th>ReportClassificationTester</th></tr>
         |		<tr><td>Epochs</td><td>100 / 100</td></tr>
         |		<tr><td>xTrain</td><td>(2,2)</td></tr>
         |		<tr><td>yTrain</td><td>(2,1)</td></tr>
         |		<tr><td>xTest</td><td>(2,3)</td></tr>
         |		<tr><td>yTest</td><td>(3,1)</td></tr>
         |		<tr><td>param</td><td>18</td></tr>
         |		<tr><td>Accuracy Type</td><td>F1-Score</td></tr>
         |		<tr><td>Loss Type</td><td>Binary Cross Entropy</td></tr>
         |		<tr><td>Train Accuracy</td><td>0.1</td></tr>
         |		<tr><td>Train Loss</td><td>1.0</td></tr>
         |		<tr><td>Test Accuracy</td><td>0.8</td></tr>
         |		<tr><td>Test Loss</td><td>0.9</td></tr>
         |	</table>
         |	<p>Resume</p>
         |</div><div class="imgBlock">
         |  <figure>
         |    <img src="static/img/$trainAccuracyImgName" alt="Train Accuracy" width="700" height="400" />
         |    <figcaption>Train Accuracy</figcaption>
         |  </figure>
         |</div><div class="imgBlock">
         |  <figure>
         |    <img src="static/img/$trainLossImgName" alt="Train Loss" width="700" height="400" />
         |    <figcaption>Train Loss</figcaption>
         |  </figure>
         |</div>
         |<div class="tableBlock">
         |	<table>
         |		<tr><th>labels</th><th>cat</th><th>dog</th></tr>
         |		<tr><td>expected</td><td>1</td><td>1</td></tr>
         |		<tr><td>cat predict</td><td>1</td><td>0</td></tr>
         |		<tr><td>dog predict</td><td>1</td><td>0</td></tr>
         |	</table>
         |	<p>Train Expected / Predictions</p>
         |</div><div class="imgBlock">
         |  <figure>
         |    <img src="static/img/$testAccuracyImgName" alt="Test Accuracy" width="700" height="400" />
         |    <figcaption>Test Accuracy</figcaption>
         |  </figure>
         |</div><div class="imgBlock">
         |  <figure>
         |    <img src="static/img/$testLossImgName" alt="Test Loss" width="700" height="400" />
         |    <figcaption>Test Loss</figcaption>
         |  </figure>
         |</div>
         |<div class="tableBlock">
         |	<table>
         |		<tr><th>labels</th><th>cat</th><th>dog</th></tr>
         |		<tr><td>expected</td><td>1</td><td>2</td></tr>
         |		<tr><td>cat predict</td><td>1</td><td>0</td></tr>
         |		<tr><td>dog predict</td><td>2</td><td>0</td></tr>
         |	</table>
         |	<p>Test Expected / Predictions</p>
         |</div>
         |</body>
         |</html>""".stripMargin)

    // after

    new Directory(new File("src/test/resources/report/ReportClassificationTest2")).deleteRecursively()

  }

}
