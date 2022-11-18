package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix
import breeze.plot.{Figure, Plot}
import org.scalatest.PrivateMethodTester
import org.scalatest.flatspec.AnyFlatSpec

import java.io.File
import java.lang.reflect.Field

class Plotter2DTest extends AnyFlatSpec with PrivateMethodTester {

  "Plotter2D.setNewFigure" should "initialize nex figure" in {

    // given

    val figure: Field = Plotter2D.getClass.getDeclaredField("figure")
    figure.setAccessible(true)

    // when

    Plotter2D.setNewFigure("figure")

    // then

    assert(figure.get(Plotter2D).asInstanceOf[Figure] != null)

  }

  "Plotter2D.addNewPlot" should "add plot in figure" in {

    // given

    val figure: Field = Plotter2D.getClass.getDeclaredField("figure")
    figure.setAccessible(true)
    Plotter2D.setNewFigure("figure")

    // when

    Plotter2D.addNewPlot("x1", "x2")

    // then

    assert(figure.get(Plotter2D).asInstanceOf[Figure].rows == 2)

  }

  "Plotter2D.addSeries" should "series in plotBuffer" in {

    // given

    val plotBuffer: Field = Plotter2D.getClass.getDeclaredField("plotBuffer")
    plotBuffer.setAccessible(true)
    Plotter2D.setNewFigure("figure")
    Plotter2D.addNewPlot("x1", "x2")

    // when

    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)))
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)), '.', Option("black"))

    // then

    assert(plotBuffer.get(Plotter2D).asInstanceOf[Plot].plot.getDatasetCount == 2)

  }

  "Plotter2D.display" should "generate png" in {

    // given

    Plotter2D.setNewFigure("figure")
    Plotter2D.addNewPlot("x1", "x2")
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)))
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)), '.', Option("black"))
    Plotter2D.addNewPlot("x1", "x2")
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)))
    Plotter2D.addSeries(DenseMatrix((1.0, 2.0, 3.0), (1.0, 2.0, 3.0)), '.', Option("black"))

    // when

    Plotter2D.display("src/test/resources/report", "MyTestPlot")
    val outputDirectory: String = new File("src/test/resources/report").listFiles.filter(_.getName.contains("MyTestPlot"))(0).getAbsolutePath

    // then

    assert(outputDirectory.split("/")(outputDirectory.split("/").length - 1).length == 28)

    // after

    new File(outputDirectory).delete()

  }

}
