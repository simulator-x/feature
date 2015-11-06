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

package simx.components.ai.mipro.supervisedlearning

import java.io.File

import simx.components.ai.mipro.Processor
import simx.components.ai.mipro.helper.PredictionQualityAnalyzer
import simx.components.ai.mipro.supervisedlearning.examples.NeuralNetwork

/**
 * Created by
 * martin
 * in September 2015.
 */
abstract class NeuralNetworkProcessor extends Processor with PredictionQualityAnalyzer {

  private var networkFolder: Option[File] = None
  private var networkImplementation: Option[Class[_ <: NeuralNetwork]] = None
  private var networkInstance: Option[NeuralNetwork] = None

  protected def network: NeuralNetwork = {
    networkInstance match {
      case Some(n: NeuralNetwork) => n
      case None =>
        val folder = networkFolder.getOrElse(throw new Exception(
          "[NeuralNetworkProcessor] network folder not set before the first access to the network instance"))
        val clazz = networkImplementation.getOrElse(throw new Exception(
          "[NeuralNetworkProcessor] network folder not set before the first access to the network instance"))
        networkInstance = Some(clazz.getConstructor(classOf[File]).newInstance(folder))
        networkInstance.get
    }
  }

  object Uses {
    def network(what: implemented.type) = new {
      def by[T <: NeuralNetwork](implementation: Class[T]): Unit = {
        networkImplementation = Some(implementation)
      }
    }
  }

  object Stores {
    def networkConfiguration(where: in.type) = new {
      def folder(folder: File): Unit = {
        networkFolder = Some(folder)
      }
    }
  }

  case object implemented
  case object in
}
