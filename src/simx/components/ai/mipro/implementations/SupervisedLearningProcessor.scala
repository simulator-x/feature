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

package simx.components.ai.mipro.implementations

import org.encog.ml.data.basic.BasicMLData
import simplex3d.math.float.ConstVec3
import simplex3d.math.floatx.ConstVec3f
import simx.components.ai.feature.recording.Events
import simx.components.ai.feature.recording.storage.Persistence
import simx.components.ai.feature.sl_chris.AnnotationReader
import simx.components.ai.mipro.Processor
import simx.components.ai.mipro.helper.PredictionQualityAnalyzer
import simx.components.ai.mipro.supervisedlearning._
import simx.components.ai.mipro.supervisedlearning.slprovider.{SlFactory, SlConfiguration}
import simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork.{NeuralNetworkConfiguration, NeuralNetwork}
import simx.core.entity.description.{SVal, SyncTime, Interpolator}
import simx.core.ontology.functions.Interpolators
import simx.core.ontology.types
import simx.components.ai.mipro.supervisedlearning.helper.Conversion.toList
import simx.core.svaractor.semantictrait.base.Thing


class SupervisedLearningProcessor(val mode: Mode = Predict())
  extends Processor with PredictionQualityAnalyzer {

  private var network: Option[NeuralNetwork] = None
  private var gestureName: Option[String] = None
  private val annotations: Option[AnnotationReader] = mode match {
    case m: PlaybackMode => Some(m.generateAnnotationReader())
    case _=> None
  }

  var magic: List[Float] = Nil

  def prediction() = {
    val data = allDataSorted
    magic = Nil
    data.foreach{g =>

      def interpolate[T,S <: Thing](sval: SVal[T,_,_,S])(implicit sync: SyncTime) = {
        val interpolator = new simx.core.ontology.functions.DataTypeBasedLinearInterpolator[T,S]
        interpolator((sval.value, sval.timestamp) :: sval.getHistory, sync.t0)
      }

      //interpolator((this.value, this.timestamp) :: getHistory, t)
      val v = localData(g._1).firstSValFor(g._2)
      //TODO: Think about revised implementation that does not create new interpolator instances every tick
      //TODO: E.g. sort keys for access once and therby store interpolators as well
      //TODO: Old line was 'val x = v.value match {'
      //TODO: Old line did not ensure temporal synchronisation
      val x = interpolate(v) match {
        case a: ConstVec3 => convert(a)
        case a: Float => List(a)
        case a: List[_] if a.forall{ case _: ConstVec3 => true; case _ => false } =>
          a.flatMap(e => convert(e.asInstanceOf[ConstVec3]))
        case a: List[_] if a.forall{ case _: Double => true; case _ => false } => a.map(_.asInstanceOf[Double].toFloat)
        case _ => List()
      }
       magic :::= x
    }
    isGestureWith(magic.map(_.toDouble))
  }

  private def convert(i: ConstVec3)= List(i.x,i.y,i.z)

  def isGestureWith(inputFeatures: List[Double]) = {
    //slData.isTrain && slData.isFromRecording
    if(mode.isInstanceOf[RecordTrain]) {
      network.get.appendTrainingData(inputFeatures)(annotations.get.gesture(gestureName.get).isPresent)
      types.Real(toFloat(annotations.get.gesture(gestureName.get).isPresent))
    } else {
      val activation = network.get.predict(new BasicMLData(inputFeatures.toArray)).head
      val gestureIsPresent: Boolean = activation > 0.5f
      //slData.isTest && slData.isFromRecording
      if(mode.isInstanceOf[RecordTest]) {
        network.get.appendTestData(inputFeatures)(annotations.get.gesture(gestureName.get).isPresent)
        addPrediction(gestureIsPresent, annotations.get(gestureName.get).isPresent)
      }
      types.Confidence(activation.toFloat)
    }
  }

  private def toFloat(bool: Boolean) = if(bool) 1f else 0f

  Reacts to Events.playbackFinished by {
   //slData.isTrain && slData.isFromRecording
   if(mode.isInstanceOf[RecordTrain]){
     network.get.saveNetwork()
//     network.get.trainNetwork()
   }
   //slData.isTest && slData.isFromRecording
   if(mode.isInstanceOf[RecordTest]) {
     println("[SupervisedLearningProcessor] Accuracy on " + network.get.name +" = " + network.get.testOnTestSet())
     network.get.saveNetwork()
     printF1Score()
   }//qualityAnalyzer.printAccuracy()
  }

  object IsConfigured{
    def by(conf: SlConfiguration): Unit ={
      conf match {
        case c: NeuralNetworkConfiguration =>
          network = Some(SlFactory.getNeuralNetworkInstance(c))
          gestureName = Some(c.getQualifiedName)
          //slData.isTrain && slData.isFromRecording
          if(mode.isInstanceOf[RecordTrain]) {
            network.foreach(n => n.loadTrainData())
          }
          //slData.isTest && !slData.isFromRecording
          if(mode.isInstanceOf[Test]) {
            network.foreach{n =>
              n.loadTestData()
              println("[SupervisedLearningProcessor] Accuracy on " + n.name + " = " + n.testOnTestSet())
            }
          }
          //slData.isTrain && !slData.isFromRecording
          if(mode.isInstanceOf[Train]) {
            network.foreach{n => {
              n.loadTrainData()
              n.trainNetwork()
            }}
          }
        case _ => println("[SupervisedLearningProcessor] SlConfiguration not recognized ")
      }
    }
  }
}
