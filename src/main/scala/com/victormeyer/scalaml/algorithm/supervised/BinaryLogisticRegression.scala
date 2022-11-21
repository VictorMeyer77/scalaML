package com.victormeyer.scalaml.algorithm.supervised

import breeze.linalg.{*, DenseMatrix}
import breeze.stats.mean
import com.victormeyer.scalaml.Logging
import com.victormeyer.scalaml.algorithm.{Accuracies, Activations, Losses}
import com.victormeyer.scalaml.model.algorithm.BinaryLogisticRegressionModel
import com.victormeyer.scalaml.report.ReportClassification

/** Classification algorithm.
 * Compute Binary Logistic Regression.
 *
 * @param xTrain Train dataset
 * @param yTrain Train expected
 * @param xTest Test dataset
 * @param yTest Test expected
 * @param classes Map of classes with value and string label
 * @param epochs Number of iterations
 * @param alpha Learning rate
 * @param reportDirectory Output folder path
 * @param reportFrequency Frequency of epochs for generate report. If 0, report will generate at the end
 */
class BinaryLogisticRegression(val xTrain: DenseMatrix[Double] = null,
                               val yTrain: DenseMatrix[Double] = null,
                               val xTest: DenseMatrix[Double] = null,
                               val yTest: DenseMatrix[Double] = null,
                               val classes: Map[Double, String] = Map(),
                               val epochs: Int = 1000,
                               val alpha: Double = 0.1,
                               val reportDirectory: String = "target/report",
                               val reportFrequency: Int = 0)
  extends SupervisedLearning with Logging with ReportClassification {

  /** Apply a binary logistic regression with binary cross entropy as loss and f1 score as accuracy
   *
   */
  override def fit(): Unit ={
    val weights: DenseMatrix[Double] = DenseMatrix.zeros(1, xTrain.cols)
    var bias: Double = 0.0
    for(i <- 0 until epochs){
      val trainPredictionsSigmoid: DenseMatrix[Double] = applyModel(weights, bias, xTrain)
      val trainLoss: Double = Losses.binaryCrossEntropy(yTrain, trainPredictionsSigmoid)
      val trainAccuracy: Double = Accuracies.f1Score(yTrain, matrixToBinary(trainPredictionsSigmoid))
      val (errorWeights, errorBias): (DenseMatrix[Double], Double) = gradientDescent(xTrain, yTrain, trainPredictionsSigmoid)
      trainAccuracies += trainAccuracy
      trainLosses += trainLoss
      if(xTest != null && yTest != null){
        val testPredictionsSigmoid: DenseMatrix[Double] = applyModel(weights, bias, xTest)
        val testPredictions: DenseMatrix[Double] = matrixToBinary(testPredictionsSigmoid)
        val testLoss: Double = Losses.binaryCrossEntropy(yTrain, testPredictionsSigmoid)
        val testAccuracy: Double = Accuracies.f1Score(yTest, testPredictions)
        testAccuracies += testAccuracy
        testLosses += testLoss
        logger.info(s"Epochs: $i - Train Accuracy: $trainAccuracy - Train Loss: $trainLoss - Test Accuracy: $testAccuracy - Test Loss: $testLoss")
      } else {
        logger.info(s"Epochs: $i - Train Accuracy: $trainAccuracy - Train Loss: $trainLoss")
      }
      if(reportFrequency > 0 && i > 0 && i % reportFrequency == 0){
        model = new BinaryLogisticRegressionModel(weights.toArray, bias)
        report((i + 1, epochs), "Binary Cross Entropy", "F1-Score")
      }
      weights -= (errorWeights.toDenseMatrix * alpha)
      bias -= (errorBias * alpha)
    }
    model = new BinaryLogisticRegressionModel(weights.toArray, bias)
    report((epochs, epochs), "Binary Cross Entropy", "F1-Score")
  }

  /** Apply model on a matrix
   *
   * @param x Matrix to predict
   * @return Matrix of predictions
   */
  override def predict(x: DenseMatrix[Double]): DenseMatrix[Double] ={
    val model: BinaryLogisticRegressionModel = this.model.asInstanceOf[BinaryLogisticRegressionModel]
    val weights: DenseMatrix[Double] = DenseMatrix(model.weights)
    matrixToBinary(applyModel(weights, model.bias, x))
  }

  /** Apply weights and bias on a matrix and compute sigmoid
   *
   * @param weights Weights of the model
   * @param bias Bias of the model
   * @param input Matrix to predict
   * @return Matrix of sigmoid
   */
  private def applyModel(weights: DenseMatrix[Double], bias: Double, input: DenseMatrix[Double]): DenseMatrix[Double] ={
    val probabilities: DenseMatrix[Double] = (weights * input.t) + bias
    Activations.sigmoid(probabilities)
  }

  /** Compute gradient descent of weights and bias
   *
   * @param input Train dataset
   * @param expected Train expected
   * @param predictions Train predictions
   * @return Tuple of weights and bias decrement
   */
  private def gradientDescent(input: DenseMatrix[Double],
                              expected: DenseMatrix[Double],
                              predictions: DenseMatrix[Double]): (DenseMatrix[Double], Double) ={
    val yDiff: DenseMatrix[Double] = predictions - expected
    val biasGradient: Double = mean(yDiff)
    val weightGradients: DenseMatrix[Double] = input.t * yDiff
    (weightGradients(*, ::).map(row => mean(row)).toDenseMatrix, biasGradient)
  }

  /** Round matrix to 0.0 or 1.0
   *
   * @param matrix Matrix to round
   * @return Rounded Matrix
   */
  private def matrixToBinary(matrix: DenseMatrix[Double]): DenseMatrix[Double] ={
    matrix.map(value =>
      if(value > 0.5)
        1.0
      else
        0.0
    )
  }

  /** Gather binary logistic regression metadata
   *
   *  @return Map of metadata, with key as metadata name and value as metadata value
   */
  override def metadata: Map[String, String] ={
    Map("alpha" -> alpha.toString)
  }

}