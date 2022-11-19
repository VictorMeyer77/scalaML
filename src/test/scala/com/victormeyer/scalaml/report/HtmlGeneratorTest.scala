package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.lang.reflect.Field
import scala.collection.mutable
import scala.io.{BufferedSource, Source}

class HtmlGeneratorTest extends AnyFlatSpec {

  "HtmlGenerator.addImageContent" should "add image block in content buffer" in {

    // given

    HtmlGenerator.setNewHtml()
    val contentBuffer: Field = HtmlGenerator.getClass.getDeclaredField("contentBuffer")
    contentBuffer.setAccessible(true)
    Plotter2D.setNewFigure("HtmlTestPlot")
    Plotter2D.addNewPlot("xTest", "yTest")
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)))
    val imgName: String = Plotter2D.display("src/test/resources/report", "htmlImgTest")

    // when

    HtmlGenerator.addImageContent("image test", imgName)

    // then

    assert(contentBuffer.get(HtmlGenerator).asInstanceOf[mutable.ArrayBuffer[String]](0) ==
    s"""<div class="imgBlock">
      |  <figure>
      |    <img src="$imgName" alt="image test" width="700" height="400" />
      |    <figcaption>image test</figcaption>
      |  </figure>
      |</div>""".stripMargin)

    // after

    new File(s"src/test/resources/report/$imgName").delete()

  }

  "HtmlGenerator.addTableContent" should "add table in content buffer" in {

    // given

    HtmlGenerator.setNewHtml()
    val contentBuffer: Field = HtmlGenerator.getClass.getDeclaredField("contentBuffer")
    contentBuffer.setAccessible(true)
    val table: DenseMatrix[String] = DenseMatrix(("label 1", "label 2"), ("col 1", "col 2"))

    // when

    HtmlGenerator.addTableContent("table test", table)

    // then

    assert(contentBuffer.get(HtmlGenerator).asInstanceOf[mutable.ArrayBuffer[String]](0) ==
    """
      |<div class="tableBlock">
      |	<table>
      |		<tr><th>label 1</th><th>label 2</th></tr>
      |		<tr><td>col 1</td><td>col 2</td></tr>
      |	</table>
      |	<p>table test</p>
      |</div>""".stripMargin)

  }

  "HtmlGenerator.generate" should "build and create html file withe all content buffer" in {

    // given

    HtmlGenerator.setNewHtml()

    Plotter2D.setNewFigure("HtmlTestPlot")
    Plotter2D.addNewPlot("xTest", "yTest")
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)))
    val imgName: String = Plotter2D.display("src/test/resources/report", "htmlImgTest")
    val table: DenseMatrix[String] = DenseMatrix(("label 1", "label 2"), ("col 1", "col 2"))

    HtmlGenerator.addTableContent("table test 1", table)
    HtmlGenerator.addImageContent("image test 1", imgName)
    HtmlGenerator.addTableContent("table test 2", table)
    HtmlGenerator.addImageContent("image test 2", imgName)

    // when

    HtmlGenerator.generate("src/test/resources/report", "MyTestHtmlReport")
    val fileBuffer: BufferedSource = Source.fromFile("src/test/resources/report/MyTestHtmlReport.html")
    val fileContent: String = fileBuffer.mkString
    fileBuffer.close()

    // then

    assert(fileContent ==
    s"""<!DOCTYPE html>
      |<html>
      |<head>
      |<title>MyTestHtmlReport</title>
      |<style>
      |table, th, td {
      |  border: 1px solid;
      |}
      |</style>
      |</head>
      |<body>
      |<h1>MyTestHtmlReport</h1>
      |
      |<div class="tableBlock">
      |	<table>
      |		<tr><th>label 1</th><th>label 2</th></tr>
      |		<tr><td>col 1</td><td>col 2</td></tr>
      |	</table>
      |	<p>table test 1</p>
      |</div><div class="imgBlock">
      |  <figure>
      |    <img src="$imgName" alt="image test 1" width="700" height="400" />
      |    <figcaption>image test 1</figcaption>
      |  </figure>
      |</div>
      |<div class="tableBlock">
      |	<table>
      |		<tr><th>label 1</th><th>label 2</th></tr>
      |		<tr><td>col 1</td><td>col 2</td></tr>
      |	</table>
      |	<p>table test 2</p>
      |</div><div class="imgBlock">
      |  <figure>
      |    <img src="$imgName" alt="image test 2" width="700" height="400" />
      |    <figcaption>image test 2</figcaption>
      |  </figure>
      |</div>
      |</body>
      |</html>""".stripMargin)

    // after

    new File(s"src/test/resources/report/$imgName").delete()
    new File("src/test/resources/report/MyTestHtmlReport.html").delete()

  }

}
