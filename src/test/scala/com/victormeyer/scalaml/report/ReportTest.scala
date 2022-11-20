package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.lang.reflect.Field
import scala.collection.mutable
import scala.reflect.io.Directory

case class ReportTester() extends Report

class ReportTest extends AnyFlatSpec {

  "Report.initReport" should "initialize report environment" in {

    // given

    val outputDirectory: String = "src/test/resources/report/initReportTest"

    // when

    ReportTester().initReport(outputDirectory)

    // then

    assert(scala.reflect.io.File(outputDirectory + "/static/img").exists)

    // after

    new Directory(new File("src/test/resources/report/initReportTest")).deleteRecursively()

  }

  "Report.add2dGraphic" should "create image block" in {

    // given

    val report: ReportTester = ReportTester()
    report.initReport("src/test/resources/report/initReportTest")

    // when

    report.add2dGraphic("title", Array(1.0, 2.0), Array(1.0, 2.0), "x", "y")

    // then

    assert(new File("src/test/resources/report/initReportTest/static/img").listFiles().nonEmpty)

    // after

    new Directory(new File("src/test/resources/report/initReportTest")).deleteRecursively()

  }

  "Report.addTable" should "add table in content buffer" in {

    // given

    val report: ReportTester = ReportTester()
    report.initReport("src/test/resources/report/initReportTest")
    val contentBuffer: Field = HtmlGenerator.getClass.getDeclaredField("contentBuffer")
    contentBuffer.setAccessible(true)
    val table: DenseMatrix[String] = DenseMatrix(("label 1", "label 2"), ("col 1", "col 2"))

    // when

    report.addTable("table test", table)

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

    // after

    new Directory(new File("src/test/resources/report/initReportTest")).deleteRecursively()

  }

  "Report.generate" should "generate report html" in {

    // given

    val report: ReportTester = ReportTester()
    report.initReport("src/test/resources/report/initReportTest")

    // when

    report.generate("MyReport")

    // then

    assert(scala.reflect.io.File("src/test/resources/report/initReportTest/MyReport.html").exists)

    // after

    new Directory(new File("src/test/resources/report/initReportTest")).deleteRecursively()

  }

}
