/*
 * Copyright 2015 The SIRIS Project
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *        http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 *
 * The SIRIS Project is a cooperation between Beuth University, Berlin and the
 * HCI Group at the University of Würzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.ai.mipro.helper

/**
 * Created by
 * martin
 * in September 15.
 */
trait PredictionQualityAnalyzer {

  private case class Prediction(prediction: Boolean, groundTruth: Boolean) {
    def isTruePositive  =  groundTruth &&  prediction
    def isTrueNegative  = !groundTruth && !prediction
    def isFalsePositive = !groundTruth &&  prediction
    def isFalseNegative =  groundTruth && !prediction
  }

  private var predictions = List[Prediction]()

  protected def addPrediction(prediction: Boolean, groundTruth: Boolean): Unit = {
    predictions ::= Prediction(prediction, groundTruth)
  }

  /**
   * Calculates the accuracy of the binary classifications added via [[PredictionQualityAnalyzer.addPrediction()]]
   * @note Cf. http://blogs.msdn.com/b/andreasderuiter/archive/2015/02/09/performance-measures-in-azure-ml-accuracy-precision-recall-and-f1-score.aspx
   */
  protected def calculateAccuracy(): Float = {
    val truePositives:  Float = predictions.count(_.isTruePositive)
    val trueNegatives:  Float = predictions.count(_.isTrueNegative)

    val correctPredictions = truePositives + trueNegatives
    val allPredictions     = predictions.size.toFloat

    correctPredictions / allPredictions
  }

  protected def printAccuracy(): Unit = {
    println(
      "[info][PredictionQualityAnalyzer] Calculated " + (calculateAccuracy() * 100f) + "% accuracy " +
      "based on " + predictions.size + " predictions."
    )
  }

  /**
   * Calculates the F1 score of the binary classifications added via [[PredictionQualityAnalyzer.addPrediction()]]
   * @note Cf. https://en.wikipedia.org/wiki/F1_score
   */
  protected def calculateF1Score(): Float = {
    val truePositives:  Float = predictions.count(_.isTruePositive)
    val falsePositives: Float = predictions.count(_.isFalsePositive)
    val falseNegatives: Float = predictions.count(_.isFalseNegative)

    val precision: Float = truePositives / (truePositives + falsePositives)
    val recall: Float = truePositives / (truePositives + falseNegatives)

    2f * ((precision * recall) / (precision + recall))
  }

  /**
   * Prints the calculated F1 score.
   * For further information regarding the interpretation of the F1 score
   * @see http://www.monperrus.net/martin/understanding+the+f1-score
   * @see http://aimotion.blogspot.de/2011/05/evaluating-recommender-systems.html
   * @see http://binf.gmu.edu/mmasso/ROC101.pdf
   */
  protected def printF1Score(): Unit = {
    println(
      "[info][PredictionQualityAnalyzer] Calculated a F1 score of" + calculateF1Score() +
        "based on " + predictions.size + " predictions."
    )
  }
}
