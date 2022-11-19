package com.victormeyer.scalaml.report

import breeze.linalg.DenseMatrix

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.collection.mutable

/** Object use to create a html file
 *
 */
object HtmlGenerator {

  private val contentBuffer: mutable.ArrayBuffer[String] = new mutable.ArrayBuffer[String]()

  /** Clear content buffer
   *
   */
  def setNewHtml(): Unit ={
    contentBuffer.clear()
  }

  /** Add an image to the html
   *
   * @param legend Legend of the image
   * @param imagePath Path of the image
   */
  def addImageContent(legend: String, imagePath: String): Unit ={
    val bloc: String =
      s"""<div class="imgBlock">
         |  <figure>
         |    <img src="$imagePath" alt="$legend" width="700" height="400" />
         |    <figcaption>$legend</figcaption>
         |  </figure>
         |</div>""".stripMargin
    contentBuffer += bloc
  }

  /** Add a table to the html
   *
   * @param legend Legend of the table
   * @param content Matrix to print as table, each matrix row is a table row
   */
  def addTableContent(legend: String, content: DenseMatrix[String]): Unit ={
    val tableHtml: StringBuilder = new StringBuilder("\n<div class=\"tableBlock\">\n\t<table>")
    tableHtml.append(content(0, ::).t.toArray.foldLeft("\n\t\t<tr>")((acc, label) => acc + s"<th>$label</th>") + "</tr>")
    for(i <- 1 until content.rows){
      tableHtml.append(content(i, ::).t.toArray.foldLeft("\n\t\t<tr>")((acc, label) => acc + s"<td>$label</td>") + "</tr>")
    }
    tableHtml.append(s"\n\t</table>\n\t<p>$legend</p>\n</div>")
    contentBuffer += tableHtml.mkString
  }

  /** Generate a simple html file with all content from contentBuffer
   *
   * @param outputDirectory Directory where create the html file
   * @param title Title and file name
   */
  def generate(outputDirectory: String="target/html", title: String): Unit ={
    val html: String = s"""<!DOCTYPE html>
                          |<html>
                          |<head>
                          |<title>$title</title>
                          |<style>
                          |table, th, td {
                          |  border: 1px solid;
                          |}
                          |</style>
                          |</head>
                          |<body>
                          |<h1>$title</h1>
                          |${contentBuffer.mkString}
                          |</body>
                          |</html>""".stripMargin
    Files.write(Paths.get(Paths.get(outputDirectory, title + ".html").toString), html.getBytes(StandardCharsets.UTF_8))
  }

}
