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

package simx.components.ai.feature.recording.storage

import java.io.{File, FileInputStream, ObjectInputStream}

import simx.components.ai.feature.recording.storage.xml.{DomSerialization, PullParsingSerialization, SemanticTypeRegistry}
import simx.core.helper.{IO, Jfx}

/**
 * Created by
 * martin
 * in July 2015.
 */
object Persistence extends DomSerialization with PullParsingSerialization with SemanticTypeRegistry {

  print_to_console = true

  def main(args: Array[String]): Unit = {
    convert()
    //testDomParsing()
    //testPullParsing()
    sys.exit(0)
  }

  def convert(): Unit = {
    while (true) {
      convertToXml(Jfx.askForFile("Choose storage file to convert", Some("bin")).get)
    }
  }

  def testDomParsing(): Unit = {
    fromXml(Jfx.askForFile("Choose storage file to load", Some("xml")).get)
  }

  def testPullParsing(): Unit = {
    lookUpSemanticType(SemanticTypeReference("position"))
    val f = Jfx.askForFile("Choose storage file to load", Some("xml")).get
      //new File("/Users/martin/Desktop/2015-ss-ml-sample-recordings-validation-set-2015-07-13/kinect/pointing/entityRecording-2015-07-07-12-23-24.xml")
    println("Pre init done")

    pullParseFromXml(f)
  }
  
  def load(storageFile: File, usePullParsing: Boolean): Storage = {
    println("[info][Persistence] Loading File: " + storageFile.getAbsolutePath)
    val (_, extension) = IO.split(storageFile)
    extension match {
      case Some("xml") =>
        if(usePullParsing) Persistence.pullParseFromXml(storageFile) else PersistenceOptimized.fromXml(storageFile)
      case Some("bin") =>
        val ooi = new ObjectInputStream(new FileInputStream(storageFile))
        ooi.readObject().asInstanceOf[Storage]
      case Some(thing) =>
        throw new Exception("[error][Persistence] Unsupported file extension '" + thing + "'")
      case None =>
        throw new Exception("[error][Persistence] Unsupported file")
    }    
  }

  def load(storageFile: File): Storage =
    load(storageFile, usePullParsing = false)

  def load(storageFiles: Seq[File], usePullParsing: Boolean): Storage = {
    val storages = storageFiles.map(load(_, usePullParsing))
    storages.reduceLeft(merge)
  }

  def load(storageFiles: Seq[File]): Storage =
    load(storageFiles, usePullParsing = true)

  /**
   * Merges s2 into s1, i.e. puts values of properties in s2 after corresponding values of properties in s1
   */
  private def merge(s1: Storage, s2: Storage): Storage = {
    s2.entities.foreach{ storedEntity2 =>
      val storedEntity1 = s1.entities.find(_.name == storedEntity2.name).getOrElse(throw
        new Exception("[error][Persistence] Recordings are not compatible. No corresponding entity for '" +
          storedEntity2.name + "' found"))

      storedEntity2.properties.foreach{storedProperty2 =>
        val storedProperty1 = storedEntity1.properties.find(_.info == storedProperty2.info).getOrElse(throw
          new Exception("[error][Persistence] Recordings are not compatible. No corresponding property for '" +
            storedProperty2.info + "' of entity '" + storedEntity2.name + "' found"))

        def mergeProperties[T](sp: StoredProperty[T], toMerge: StoredProperty[_]): Unit = {
          sp.values = sp.values ::: toMerge.values.asInstanceOf[List[(T, Long)]]
        }

        mergeProperties(storedProperty1, storedProperty2)
      }
    }
    s1.metaData = s1.metaData ::: s2.metaData
    s1
  }

}
