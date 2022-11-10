package com.victormeyer.scalaml.algorithm.supervised

import com.victormeyer.scalaml.data.Matrix
import com.victormeyer.scalaml.model.{Model, ModelFile}

/** Trait which must implemented by all supervised algorithm
 *
 * @tparam T Type of matrix
 */
trait SupervisedLearning[T] {

  var model: Model = _
  var xTrain: Matrix[T] = _
  var yTrain: Matrix[T] = _

  /** Load model from file
   *
   * @param path Path of the json file
   */
  def loadModel(path: String): Unit ={
    model = ModelFile.loadModel(path)
  }

  /** Save model on disk
   *
   * @param outputDirectory Path of folder to save model
   * @param modelName Name of the model
   */
  def saveModel(outputDirectory: String, modelName: String): Unit ={
    ModelFile.saveModel(outputDirectory, this.getClass.getSimpleName, modelName, model)
  }

  /** Set train data
   *
   * @param xTrain Exogenous variables
   * @param yTrain Endogenous variables
   */
  def setData(xTrain: Matrix[T], yTrain: Matrix[T]): Unit ={
    this.xTrain = xTrain
    this.yTrain = yTrain
  }

  /** Compute training
   *
   * @param numeric Numeric instance
   */
  def fit(implicit numeric: Numeric[T]): Unit

  /** Apply model on matrix
   *
   * @param x Matrix to predict
   * @return
   */
  def predict(x: Matrix[T]): Matrix[Double]

}
