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
import simx.components.ai.feature.recording.{StartFastForwardPlayback, EntityPlayer}
import simx.core.helper.{Jfx, IO}
import simx.core.svaractor.SVarActor

/**
 * Created by chris on 21/07/15.
 */
trait SupervisedLearning extends SVarActor{


  private var entityPlayer: Option[SVarActor.Ref] = None


  private val task: Task = getTask(IO.askForOptions("What do you wanna do ? ", "Train", "Test", "Predict"))

  var fromRecording = task match {
    case  Train() | Test() => if(IO.askForOptions("Load from entity recording ?", "Yes", "No") == 0) true else false
    case _ => false
  }

  var playbackFile: Option[File] = task match {
    case Train() if fromRecording => Jfx.askForFile("Choose entity recording for training")
    case Test() if fromRecording => Jfx.askForFile("Choose entity recording for testing")
    case _ => None
  }


  val playbackData = playbackFile.map(f => PersistenceOptimized.fromXml(f))

  playbackData.foreach{data => entityPlayer = Some(SVarActor.createActor(new EntityPlayer(data)))}
  val playbackAnnotationFile  = playbackFile.map(IO.changeExtension("csv"))


  val neuralNetworkFolder = Jfx.askForFolder("Select neural network folder.").get

  def startPlayback(): Unit ={
    if(slData.isFromRecording) entityPlayer.collect { case player => player ! StartFastForwardPlayback(forerunInMillis = 10000L, coolDownInMillis = 3000L, speedUp = 10L) }
  }

  implicit var slData: SLData = SLData(playbackData, playbackAnnotationFile, task)

  private def getTask(t: Int): Task = {
    t match {
      case 0 => Train()
      case 1 => Test()
      case 2 => Predict()
    }
  }
}

case class SLData(playbackData: Option[Storage],
                  annotationFile: Option[File],
                  task: Task) {

  def isFromRecording = playbackData.isDefined
  def isTrain = task.isInstanceOf[Train]
  def isTest = task.isInstanceOf[Test]
  def isPredict = task.isInstanceOf[Predict]
}

abstract class Task
case class Train() extends Task
case class Test() extends Task
case class Predict() extends Task

