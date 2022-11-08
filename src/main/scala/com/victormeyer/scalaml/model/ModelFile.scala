package com.victormeyer.scalaml.model

import com.google.gson.{Gson, GsonBuilder}
import com.victormeyer.scalaml.Logging

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Files, Paths}
import java.util.Calendar
import scala.io.{BufferedSource, Source}

/** Gather method to save and load each type of model
 *
 */
object ModelFile extends Logging {

  private val gson: Gson = new GsonBuilder().setPrettyPrinting().create()

  /** Save a model to a json file
   *
   * @param outputDirectory Directory path to save model
   * @param modelType Type of the model, which equal to class name in algorithm package
   * @param modelName Name of the model
   * @param model Model instance to save
   */
  def saveModel(outputDirectory: String, modelType: String, modelName: String, model: Model): Unit ={
    Files.createDirectories(Paths.get(outputDirectory))
    val modelWrapper: ModelWrapper = ModelWrapper(modelType, modelName, Calendar.getInstance().getTimeInMillis, model)
    val modelJson: String = gson.toJson(modelWrapper)
    val filePath: String = Paths.get(outputDirectory, s"$modelName-${modelWrapper.dateMaj}.json").toString
    val outputFile: File = new File(filePath)
    val bufferWriter: BufferedWriter = new BufferedWriter(new FileWriter(outputFile))
    bufferWriter.write(modelJson)
    bufferWriter.close()
    logger.info(s"Model $modelType successfully saved: $filePath")
  }

  /** Load model from json file
   *
   * @param path Path of file
   * @return Model instance
   */
  def loadModel(path: String): Model ={
    val fileBuffer: BufferedSource = Source.fromFile(path)
    val fileContent: String = fileBuffer.mkString
    val modelType: String = getModelType(fileContent)
    val modelJson: String = getModelJson(fileContent)
    val model: Model = gson.fromJson(modelJson, Class.forName(s"${this.getClass.getPackageName}.algorithm.${modelType}Model"))
    fileBuffer.close()
    logger.info(s"Model $modelType successfully load from file: $path")
    model
  }

  /** Parse the model type from whole json file
   *
   * @param modelString Json model in string
   * @return Model type
   */
  private def getModelType(modelString: String): String ={
    modelString.split("\n")
      .filter(row => row.contains("modelType"))
      .mkString.split(":")(1).replace("\"", "").replace(",", "").trim
  }

  /** Parse model from whole json file
   *
   * @param modelString Json model in string
   * @return Model class json
   */
  private def getModelJson(modelString: String): String ={
    val lines: Array[String] = modelString.split("\n")
    lines.slice(4, lines.length - 1).mkString("\n").replace("\"model\": ", "").trim
  }

}
