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

package simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork

import org.encog.ml.data.basic.BasicMLDataSet
import org.encog.ml.train.MLTrain
import org.encog.ml.train.strategy._
import org.encog.neural.networks.ContainsFlat
import org.encog.neural.networks.training.propagation.Propagation
import org.encog.neural.networks.training.propagation.back.Backpropagation
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation
import simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork.EncogPropagations.EncogPropagations
import simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork.EncogTrainStrategies.EncogTrainStrategies

import scala.xml.NodeSeq

/**
 * Enum holding representations of the different propagration modes contained in encog.
 */
object EncogPropagations extends Enumeration
{
  type EncogPropagations = Value
  val BackPropagation, ResilientPropagation = Value

  /**
   * Deserializes a propagation instance from XML.
   *
   * @param aStr The XML String.
   * @return
   */
  def fromString(aStr: String): Option[EncogPropagations] =
  {
    var tBack: Option[EncogPropagations] = None

    val tBackPropagationStr = BackPropagation.toString
    val tResilientPropagationStr = ResilientPropagation.toString

    aStr match
    {
      case `tBackPropagationStr` => tBack = Some(BackPropagation)
      case `tResilientPropagationStr` => tBack = Some(ResilientPropagation)
      case _ => tBack = None
    }

    tBack
  }
}

/**
 * Enum holding representations of the different train srategies contained in encog.
 */
object EncogTrainStrategies extends Enumeration
{
  type EncogTrainStrategies = Value
  val Greedy, HybridStrategy, ResetStrategy, RequiredImprovementStrategy, StopTrainingStrategy = Value

  /**
   * Deserializes a train strategy instance from XML.
   *
   * @param aStr The XML String.
   * @return
   */
  def fromString(aStr: String): Option[EncogTrainStrategies] =
  {
    var tBack: Option[EncogTrainStrategies] = None
    
    val tGreedyStr = Greedy.toString
    val tHybridStrategyStr = HybridStrategy.toString
    val tResetStrategyStr = ResetStrategy.toString
    val tRequiredImprovementStrategyStr = RequiredImprovementStrategy.toString
    val tStopTrainingStrategyStr = StopTrainingStrategy.toString

    aStr match
    {
      case `tGreedyStr`=> tBack = Some(Greedy)
      case `tHybridStrategyStr` => tBack = Some(HybridStrategy)
      case `tResetStrategyStr` => tBack = Some(ResetStrategy)
      case `tRequiredImprovementStrategyStr` => tBack = Some(RequiredImprovementStrategy)
      case `tStopTrainingStrategyStr` => tBack = Some(StopTrainingStrategy)
      case _ => tBack = None
    }

    tBack
  }
}

/**
 * Configuration object for the encog library.
 */
case class EncogConfiguration(aPropagation: EncogPropagations = EncogPropagations.ResilientPropagation,
                              aTrainError: Float = 0.01f,
                              aTrainStrategy: EncogTrainStrategies = EncogTrainStrategies.RequiredImprovementStrategy,
                              aCycles: Int = 300)
{
  private var iPropagation = aPropagation
  private var iTrainError = aTrainError
  private var iTrainStrategy = aTrainStrategy
  private var iCycles = aCycles

  def setPropagation(aPropagation: EncogPropagations) = iPropagation = aPropagation
  def setTrainError(aTrainError: Float) = iTrainError = aTrainError
  def setTrainStrategy(aTrainStrategy: EncogTrainStrategies) = iTrainStrategy = aTrainStrategy
  def setCycles(aCycles: Int) = iCycles = aCycles

  def getPropagation(): EncogPropagations = iPropagation
  def getTrainError(): Float = iTrainError
  def getTrainStrategy(): EncogTrainStrategies = iTrainStrategy
  def getCycles(): Int = iCycles

  /**
   * Creates an XML representation of an EncogConfiguration.
   * @return The XML representation.
   */
  def toXml() =
  {
    <ENCOG_CONFIG>
      <!-- Possible propagations are: BackPropagation, ResilientPropagation -->
      <PROPAGATION>{iPropagation}</PROPAGATION>
      <TRAIN_ERROR>{iTrainError}</TRAIN_ERROR>
      <!-- Possible train strategies are: Greedy, HybridStrategy, ResetStrategy, RequiredImprovementStrategy, StopTrainingStrategy -->
      <TRAIN_STRATEGY>{iTrainStrategy}</TRAIN_STRATEGY>
      <CYCLES>{iCycles}</CYCLES>
    </ENCOG_CONFIG>
  }

  /**
   * Restores an EncogConfiguration from XML.
   *
   * @param aXml A deserialized XML element.
   */
  def fromXml(aXml: NodeSeq): Unit =
  {
    if (aXml.nonEmpty)
    {
      val tRoot = aXml \\ "ENCOG_CONFIG"
      val tPropagationXml = tRoot \\ "PROPAGATION"
      val tTrainErrorXml = tRoot \\ "TRAIN_ERROR"
      val tTrainStrategyXml = tRoot \\ "TRAIN_STRATEGY"
      val tCyclesXml = tRoot \\ "CYCLES"

      val tPropagation: Option[EncogPropagations] = EncogPropagations.fromString(tPropagationXml.text)
      val tTrainError: Float = tTrainErrorXml.text.toFloat
      val tTrainStrategy: Option[EncogTrainStrategies] = EncogTrainStrategies.fromString(tTrainStrategyXml.text)
      val tCycles: Int = tCyclesXml.text.toInt

      /* Set the restored values into the corresponding members. */
      if (tPropagation.isDefined && tTrainStrategy.isDefined)
      {
        iPropagation = tPropagation.get
        iTrainError = tTrainError
        iTrainStrategy = tTrainStrategy.get
        iCycles = tCycles
      }
    }
  }

  /**
   * Returns an Option that can contain an instance of a Propagation.
   *
   * @param aNeuralNetwork A ContainsFlat instance, typically a NeuralNetwork instance.
   * @param aDataSet The BasicMLDataSet to use.
   * @return
   */
  def getPropagationInstance(aNeuralNetwork: ContainsFlat, aDataSet: BasicMLDataSet): Option[Propagation] =
  {
    var tBack: Option[Propagation] = None

    iPropagation match
    {
      case EncogPropagations.BackPropagation => tBack = Some(new Backpropagation(aNeuralNetwork, aDataSet))
      case EncogPropagations.ResilientPropagation => tBack = Some(new ResilientPropagation(aNeuralNetwork, aDataSet))
      case _ => tBack = None
    }

    tBack
  }

  /**
   * Returns an Option that can contain an instance of a Strategy.
   *
   * @param aTrain An MLTrain instance, typically a Propagation instance.
   * @return
   */
  def getTrainStrategyInstance(aTrain: MLTrain): Option[Strategy] =
  {
    var tBack: Option[Strategy] = None

    iTrainStrategy match
    {
      case EncogTrainStrategies.Greedy => tBack = Some(new Greedy())
      case EncogTrainStrategies.HybridStrategy => tBack = Some(new HybridStrategy(aTrain))
      case EncogTrainStrategies.RequiredImprovementStrategy => tBack = Some(new RequiredImprovementStrategy(aTrainError,
        0.035f, iCycles))
      case EncogTrainStrategies.ResetStrategy => tBack = Some(new ResetStrategy(iTrainError, aCycles))
      case EncogTrainStrategies.StopTrainingStrategy => tBack = Some(new StopTrainingStrategy(iTrainError, aCycles))
      case _ => tBack = None
    }

    tBack
  }

  override def toString(): String = "EncogConfiguration: Propagation - " + iPropagation + " | TrainError - " +
    iTrainError + " | TrainStrategy - " + iTrainStrategy + " | Cycles - " + iCycles
}
