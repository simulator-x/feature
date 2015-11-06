/*
 * Copyright 2014 The SIRIS Project
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

package simx.components.ai.feature

import simx.core.svaractor.SVarActor
import simx.core.worldinterface.eventhandling.{EventDescription, Event, EventProvider}
import java.io.{FileInputStream, ObjectInputStream, File}

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 05/02/14
 * Time: 10:30
 */
class InputFeaturePlayer(recordedInputFeatures: List[File], delayBetweenFilesInMillis: Long = 0)
  extends SVarActor with EventProvider {

  private var toProcess = recordedInputFeatures
  private var toEmit = List[(Long, Event)]()

  override protected def startUp() = {
    println("[info][InputFeaturePlayer] Starting to playback " + recordedInputFeatures.size + " file(s).")
    processNextFile()
  }

  def processNextFile() {
    if(toProcess.isEmpty) {
      println("[info][InputFeaturePlayer] Finished playback of " + recordedInputFeatures.size + " file(s).")
      shutdown()
    } else {
      val file = toProcess.head
      println("[info][InputFeaturePlayer] Loading recorded input features from " + file.getAbsolutePath)
      val ooi = new ObjectInputStream(new FileInputStream(file))
      val data = ooi.readObject().asInstanceOf[FeatureComponent.inputRecordingDataType]
      //Deprecated
      //data.map(timedEvent => new EventDescription(timedEvent._2.name)).distinct.foreach(provideEvent(_, None))
      toEmit = data.sortWith(_._1 < _._1)
      println("[info][InputFeaturePlayer] Starting to play back data.")
      emitNextInputFeature()
      toProcess = toProcess.tail
    }
  }

  def emitNextInputFeature() {
    toEmit match {
      case Nil =>
        println("[info][InputFeaturePlayer] The loaded data contained no recorded input features. Shutting down.")
        addJobIn(delayBetweenFilesInMillis){processNextFile()}
      case head :: Nil =>
        emitEvent(head._2)
        println("[info][InputFeaturePlayer] Playback complete.")
        addJobIn(delayBetweenFilesInMillis){processNextFile()}
      case head :: tail =>
        emitEvent(head._2)
        toEmit = tail
        addJobIn(tail.head._1 - head._1){emitNextInputFeature()}
    }
  }
}
