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
 * HCI Group at the University of Wuerzburg. The project is funded by the German
 * Federal Ministry of Education and Research (grant no. 17N4409).
 */

package simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork

import java.io.File
import org.encog.engine.network.activation.ActivationSigmoid
import org.encog.ml.data.MLData
import org.encog.ml.data.basic.BasicMLDataSet
import org.encog.ml.train.strategy.{StopTrainingStrategy, ResetStrategy}
import org.encog.neural.networks.BasicNetwork
import org.encog.neural.networks.layers.BasicLayer
import org.encog.neural.networks.training.propagation.Propagation
import org.encog.neural.networks.training.propagation.resilient.ResilientPropagation
import simx.components.ai.mipro.helper.PredictionQualityAnalyzer
import simx.components.ai.mipro.supervisedlearning.util.NeuralNetworkState
import simx.components.ai.mipro.supervisedlearning.{Predict, Task}

import scala.collection.mutable


class NeuralNetwork(aConfigurationObj: NeuralNetworkConfiguration) {
  def input_layer_size: Int = aConfigurationObj.getInputLayerSize
  val num_labels: Int = aConfigurationObj.getNumLabels
  val neuralNetworkFolder: File = aConfigurationObj.getTargetFolder
  val lambda: Float = aConfigurationObj.getLambda
  val hidden_layers = aConfigurationObj.getHiddenLayers
  def trainData: NNData = aConfigurationObj.getTrainingData
  def testData = aConfigurationObj.getTestData
  def name = aConfigurationObj.aQualifiedName
  private val weights: Option[Weights] = aConfigurationObj.getWeights


  private var network: Option[BasicNetwork] = None


  def appendTrainingData(x: List[Double], xs: List[Double]*)(y: List[Double], ys: List[Double]*): Unit = {
    val _x = x ::: xs.flatten.toList
    val _y = y ::: ys.flatten.toList
    if(_x.size != input_layer_size) aConfigurationObj.setInputLayerSize(_x.size)
    aConfigurationObj.appendTrainingData(_x.toArray, _y.toArray)
  }

  def appendTestData(x: List[Double], xs: List[Double]*)(y: List[Double], ys: List[Double]*): Unit = {
    val _x = x ::: xs.flatten.toList
    val _y = y ::: ys.flatten.toList
    if(_x.size != input_layer_size) aConfigurationObj.setInputLayerSize(_x.size)
    aConfigurationObj.appendTestData(_x.toArray, _y.toArray)
  }

  def trainNetwork() {
    println("Starting Train")
    trainNetwork(trainData.dataSet)
    println("Accuracy = " + testOnSet(trainData.dataSet))
    saveNetwork()
  }

  def loadTrainData(func: Option[() => Unit] = None): Unit = {
    aConfigurationObj.loadTrainingData()
  }

  def loadTestData(func: Option[() => Unit] = None): Unit ={
    aConfigurationObj.loadTestData()
  }


  def saveNetwork() {
    buildNetwork()
    if(aConfigurationObj.getCurrentState == NeuralNetworkState.Predict){
      val weights = getWeights(network.get)
      aConfigurationObj.setWeights(weights)
    }
    aConfigurationObj.updateToFileSystem()
  }

  def testOnTestSet(): Float = {
    testOnSet(testData.dataSet)
  }

  def testOnTrainSet(): Float = {
    testOnSet(trainData.dataSet)
  }

  def testOnSet(set: BasicMLDataSet): Float = {
    buildNetwork()

    case class Prediction(_computed: Array[Double], ideal: Array[Double]) {
      def computed = _computed.map(value => if (value > 0.5) 1.0 else 0.0)

      def isTrue = computed.sameElements(ideal)

      def score = if (isTrue) 1.0f else 0.0f
    }
    var predictions: List[Prediction] = Nil
    for (i <- 0 until set.getData.size()) {
      val pair = set.get(i)
      val output = network.get.compute(pair.getInput)

      predictions ::= Prediction(output.getData, pair.getIdeal.getData)
    }
    val accuracy = predictions.map(_.score).sum / predictions.size.toFloat
    accuracy
  }


  private def trainNetwork(data: BasicMLDataSet)
  {
    buildNetwork()
    val tPropagationOpt: Option[Propagation] = aConfigurationObj.getEncogConfig.getPropagationInstance(network.get, data)

    if (tPropagationOpt.isDefined)
    {
      val tPropagation = tPropagationOpt.get
      val tStrategyOpt = aConfigurationObj.getEncogConfig.getTrainStrategyInstance(tPropagation)
      var epoch = 1

      if (tStrategyOpt.isDefined)
      {
        tPropagation.addStrategy(tStrategyOpt.get)
        tStrategyOpt.get match {
          case strategy: StopTrainingStrategy =>
            do{doIteration()} while (!strategy.shouldStop())
          case _ =>
            do{doIteration()} while (tPropagation.getError > aConfigurationObj.getEncogConfig.getTrainError())
        }
        aConfigurationObj.setCurrentState(NeuralNetworkState.Predict)
      }

      def doIteration(): Unit = {
        tPropagation.iteration()
        System.out.println("Epoch #" + epoch + " Error:" + tPropagation.getError)
        epoch += 1
      }
    }
  }

  def predict(input: MLData): Array[Double] = {
    buildNetwork()
    network.get.compute(input).getData
  }

  private def buildNetwork() {
    if(network.isEmpty){
      val n = new BasicNetwork()

      n.addLayer(new BasicLayer(null, true, input_layer_size))

      hidden_layers.foreach{layerSize =>
        n.addLayer(new BasicLayer(new ActivationSigmoid(), true, layerSize))
      }

      n.addLayer(new BasicLayer(new ActivationSigmoid(), false, num_labels))

      n.getStructure.finalizeStructure()
      n.reset()

      if(weights.isDefined) {
        weights.get.data.foreach{weight =>
          n.setWeight(weight.fromLayer, weight.fromNeuron, weight.toNeuron, weight.value)
        }
      }
      network = Some(n)
    }

  }

  private def getWeights(network: BasicNetwork): Weights = {
    var weights: List[Weight] = Nil
    val layers = network.getLayerCount
    for (l <- 1 to layers) {
      for (n <- 1 to network.getLayerTotalNeuronCount(l - 1)) {
        if (l < layers) {
          val nInNextLayer = network.getLayerNeuronCount(l)
          for (next <- 1 to nInNextLayer) {
            val ln = l + 1
            val weight = network.getWeight(l - 1, n - 1, next - 1)
            weights ::= Weight(l - 1, n - 1, next - 1, weight)
          }
        }
      }
    }
    new Weights(weights)
  }
}




