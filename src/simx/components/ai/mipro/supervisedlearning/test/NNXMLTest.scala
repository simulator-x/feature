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

package simx.components.ai.mipro.supervisedlearning.test

import java.io.File

import simx.components.ai.mipro.supervisedlearning.slprovider.SlFactory
import simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork.{EncogTrainStrategies, EncogPropagations, EncogConfiguration, NeuralNetworkConfiguration}
import simx.components.ai.mipro.supervisedlearning.util.{ConfigurationSavingMode, SlConstants}

object NNXMLTest
{
  /**
   * Application entry
   * @param args
   */
  def main(args: Array[String])
  {
//    val tName = "testconfig"
//    val tFile = new File(SlConstants.CONFIG_SUB_DIR + "/" + tName)
//
//    val tNNConf = NeuralNetworkConfiguration(tName, tFile)
//    val network = SlFactory.getNeuralNetworkInstance(tNNConf)
//
//    network.appendTrainingData(1.0 :: 2.0 :: 3.0 :: Nil)(1.0 :: Nil)
//    network.appendTrainingData(1.0 :: 2.0 :: 3.0 :: Nil)(1.0 :: Nil)
//    network.appendTrainingData(1.0 :: 2.0 :: 3.0 :: Nil)(1.0 :: Nil)
//
//    network.appendTrainingData(2.0 :: 3.0 :: 4.0 :: Nil)(0.0 :: Nil)
//    network.appendTrainingData(2.0 :: 3.0 :: 4.0 :: Nil)(0.0 :: Nil)
//
//    network.appendTestData(1.0 :: 2.0 :: 3.0 :: Nil)(1.0 :: Nil)
//    network.appendTestData(1.0 :: 2.0 :: 3.0 :: Nil)(1.0 :: Nil)
//    network.appendTestData(1.0 :: 2.0 :: 3.0 :: Nil)(1.0 :: Nil)
//
//    network.appendTestData(2.0 :: 3.0 :: 4.0 :: Nil)(0.0 :: Nil)
//    network.appendTestData(2.0 :: 3.0 :: 4.0 :: Nil)(0.0 :: Nil)
//
//    tNNConf.setEncogConfig(EncogConfiguration(EncogPropagations.BackPropagation, 0.05f,
//      EncogTrainStrategies.HybridStrategy))
//
//    network.saveNetwork()
//
//    val tNewNNConf = NeuralNetworkConfiguration(tName, tFile)
//
//    println("Is same network = " + tNewNNConf.equals(tNNConf))
//
//    tNewNNConf.loadTrainingData()
//    println("New TrainData: ")
//    println(tNewNNConf.getTrainingData)
//    println("Old TrainData: ")
//    println(tNNConf.getTrainingData)
//
//    tNewNNConf.loadTestData()
//    println("New TestData: ")
//    println(tNewNNConf.getTestData)
//    println("Old TestData: ")
//    println(tNNConf.getTestData)
//
//    println("New EncogConfig")
//    println(tNewNNConf.getEncogConfig.toString())
//    println("Old EncogConfig")
//    println(tNNConf.getEncogConfig.toString())
//
//    val newNetwork = SlFactory.getNeuralNetworkInstance(tNewNNConf)
//    println("Accuracy: " + network.testOnTrainSet())
//    println("Accuracy: " + newNetwork.testOnTrainSet())
//
//    println(network.getWeights)
//    println(newNetwork.getWeights)
  }
}
