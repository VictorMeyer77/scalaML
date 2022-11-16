package com.victormeyer.scalaml.algorithm.supervised

import breeze.linalg.DenseMatrix
import com.victormeyer.scalaml.model.{Model, ModelFile}

/** Trait which must implemented by all supervised algorithm.
 *
 */
trait SupervisedLearning {

  var model: Model = _

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

  /** Compute training
   *
   */
  def fit(): Unit

  /** Apply model on matrix
   *
   * @param x Matrix to predict
   * @return Matrix of predictions
   */
  def predict(x: DenseMatrix[Double]): DenseMatrix[Double]

}