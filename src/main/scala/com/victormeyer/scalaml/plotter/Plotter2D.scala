package com.victormeyer.scalaml.plotter

import breeze.linalg.DenseMatrix
import breeze.plot.{Figure, Plot, Series}

import java.nio.file.{Files, Paths}
import breeze.plot._

import java.util.Calendar

/** Object use to create 2D graphic and generate .png.
 *
 */
object Plotter2D {

  private var figure: Figure = _
  private var plotBuffer: Plot = _

  /** Initialize a new figure. Should to be call at the beginning.
   *
   * @param name Name of the figure
   */
  def setNewFigure(name: String): Unit ={
    figure = Figure(name)
  }

  /** Add a new plot in the figure. All plot are placed in row. Should to be call after setNewFigure.
   *  When this method is called, update former plot is impossible.
   *
   * @param xLabel Name of abscissa axe
   * @param yLabel Name of ordinate axe
   */
  def addNewPlot(xLabel: String, yLabel: String): Unit ={
    plotBuffer = figure.subplot(figure.rows - 1)
    plotBuffer.xlabel = xLabel
    plotBuffer.ylabel = yLabel
    figure.rows += 1
  }

  /** Add new series in the current plot. Should to be call after setNewPlot.
   *
   * @param matrix Matrix with two rows representing abscissa and ordinate
   * @param style Style of the series: '-', '.' or '+'
   * @param colorName Optional: color of the series
   */
  def addSeries(matrix: DenseMatrix[Double],
                 style: Char = '-',
                 colorName: Option[String]=None): Unit ={
    val series: Series = if(colorName.isDefined) {
        plot(matrix(0, ::).t, matrix(1, ::).t, style, colorcode = colorName.get)
      } else {
        plot(matrix(0, ::).t, matrix(1, ::).t, style)
      }
    plotBuffer += series
  }

  /** Print and save the figure in a png.
   *
   * @param outputPath Output directory
   * @param plotName File name. File will be timestamping
   */
  def display(outputPath: String="target/plot", plotName: String="graphic"): Unit ={
    Files.createDirectories(Paths.get(outputPath))
    figure.rows -= 1
    figure.saveas(Paths.get(outputPath, plotName + "-" + Calendar.getInstance().getTimeInMillis).toString + ".png")
  }

}
