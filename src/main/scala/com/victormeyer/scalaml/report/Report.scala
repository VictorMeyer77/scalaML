package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix

import java.nio.file.{Files, Paths}

/** Trait which centralize methods to generate complete report.
 * This trait is implemented by all other Report trait.
 *
 */
trait Report {

  private var workingDirectory: String = ""

  /** Initialize report environment
   *
   * @param outputDirectory Folder path of the report
   */
  def initReport(outputDirectory: String="target/report"): Unit ={
    workingDirectory = outputDirectory
    Files.createDirectories(Paths.get(outputDirectory, "static/img"))
    HtmlGenerator.setNewHtml()
  }

  /** Add a 2d graphic bloc in the report
   *
   * @param title Title of the graphic
   * @param x Abscissa data
   * @param y Ordinate data
   * @param xLabel Abscissa label
   * @param yLabel Ordinate label
   */
  def add2dGraphic(title: String, x: Array[Double], y: Array[Double], xLabel: String, yLabel: String): Unit ={
    Plotter2D.setNewFigure(title)
    Plotter2D.addNewPlot(xLabel, yLabel)
    Plotter2D.addSeries(DenseMatrix(x, y))
    val outputGraphicName: String = Plotter2D.display(Paths.get(workingDirectory, "static/img").toString, title)
    HtmlGenerator.addImageContent(title, s"static/img/$outputGraphicName")
  }

  /** Wrapper of HtmlReport.addTableContent
   *
   * @param legend Legend of the table
   * @param content Matrix to print as table, each matrix row is a table row
   */
  def addTable(legend: String, content: DenseMatrix[String]): Unit ={
    HtmlGenerator.addTableContent(legend, content)
  }

  /** Wrapper of HtmlReport.generate
   *
   * @param title Title and file name
   */
  def generate(title: String): Unit ={
    HtmlGenerator.generate(workingDirectory, title)
  }

}
