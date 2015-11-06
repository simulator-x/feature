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

import simx.components.ai.feature.recording.storage.xml.{DomSerializationOptimized, PullParsingSerialization, SemanticTypeRegistry}
import simx.core.helper.Jfx


/**
 * Created by connector on 11.09.15.
 */
object PersistenceOptimized  extends DomSerializationOptimized with PullParsingSerialization with SemanticTypeRegistry {

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
}
