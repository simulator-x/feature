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

import simx.components.ai.feature.recording.storage.{PersistenceOptimized, Storage, Persistence}
import simx.components.ai.feature.recording.{FastForwardPlayback, EntityPlayer}
import simx.components.ai.feature.sl_chris.AnnotationReader
import simx.core.helper.{Jfx, IO}
import simx.core.svaractor.SVarActor

/**
*  Created by chris and martin
*  on March 2016.
*/
trait SupervisedLearning extends SVarActor{

  protected val slMode: Mode =
    IO.askForOptions("What do you wanna do?",
      "Record Training Data",
      "Record Test Data",
      "Train from Recording",
      "Test from Recording",
      "Predict"
    ) match
    {
      case 0 => RecordTrain(Jfx.askForFile("Choose entity recording for training").get)
      case 1 => RecordTest(Jfx.askForFile("Choose entity recording for testing").get)
      case 2 => Train()
      case 3 => Test()
      case 4 => Predict()
    }

  val neuralNetworkFolder = Jfx.askForFolder("Select neural network folder.").get
  
  protected def startPlayback(delay: Long = 0L): Unit = {
    slMode match {
      case t: PlaybackMode =>
        if(delay == 0L)
          _startPlayback(t.entityPlayer)
        else
          addJobIn(delay) {_startPlayback(t.entityPlayer)}
      case _ =>
    }
  }

  private def _startPlayback(player: SVarActor.Ref): Unit = {
    player ! FastForwardPlayback(forerunInMillis = 10000L, coolDownInMillis = 3000L, speedUp = 10L)
  }
}

abstract class Mode
abstract class PlaybackMode(playbackFile: File) extends Mode {
  val playbackData = PersistenceOptimized.fromXml(playbackFile)
  val entityPlayer = SVarActor.createActor(new EntityPlayer(playbackData))
  val playbackAnnotationFile  = IO.changeExtension("csv")(playbackFile)

  def generateAnnotationReader() =
    new AnnotationReader(playbackAnnotationFile, playbackData.metaData)
}
case class Train() extends Mode
case class Test() extends Mode
case class Predict() extends Mode
case class RecordTrain(playbackFile: File) extends PlaybackMode(playbackFile)
case class RecordTest(playbackFile: File) extends PlaybackMode(playbackFile)