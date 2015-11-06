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

package simx.components.ai.feature.recording.storage.xml

import java.io.File
import java.util.UUID

import com.thoughtworks.xstream.XStream
import simx.components.ai.feature.recording.storage.{Storage, StoredEntity, StoredProperty}

import scala.collection.mutable
import scala.xml.pull.{EvElemEnd, EvElemStart, EvText}

/**
 * Created by martin 
 * on 05/08/15.
 */
trait PullParsingSerialization extends PullParsing with SemanticTypeRegistry {

  def pullParseFromXml(storageFile: File): Storage = {
    println(Console.MAGENTA + "Pull parsing " + storageFile.getAbsolutePath + Console.RESET)
    val before = System.currentTimeMillis()


    val storage     = new Storage

    def onTextCompanionVideoStartTime(s: Storage)(t: EvText): Unit = {
      s.recordingStart = Some(t.text.toLong)
    }

    addEvHandler(Exactly("entityRecording", "companionVideoStartTime"), OnText(onTextCompanionVideoStartTime(storage)))
    addEvHandler(Exactly("entityRecording", "entity"), new EntityBuilder(storage))

    pullParse(storageFile)

    println(Console.GREEN + "Finished parsing file (" + (System.currentTimeMillis() - before) + "ms)" + Console.RESET)
    storage
  }

  private class RawXmlBuilder(out: mutable.Stack[StringBuilder]) extends EvHandler {
    override def onStart(s: EvElemStart) {
      val as = s.attrs.toString()
      out.top ++= "<" + s.label + as  + ">"
    }
    override def onText(t: EvText) { out.top ++= t.text }
    override def onEnd(e: EvElemEnd) { out.top ++= "</" + e.label + ">" }
  }

  private class PropertyBuilder(properties: mutable.Stack[StoredProperty[_]]) extends EvHandler {
    val semantics = new mutable.Stack[String]()
    val values    = new mutable.Stack[StringBuilder]()

    addEvHandler(Exactly("property", "semantics"), OnText(onTextSemantics))
    addEvHandler(AllBelow("values", "scala.Tuple2-array"), new RawXmlBuilder(values))

    def onTextSemantics(t: EvText): Unit = {
      semantics.push(t.text)
    }

    override def onStart(s: EvElemStart): Unit = {
      values.push(new StringBuilder())
    }

    override def onEnd(e: EvElemEnd): Unit = {
      val sem = semantics.pop()
      val info = lookUpSemanticType(SemanticTypeReference(sem)).getOrElse(throw new Exception("Failed to look up semantic type " + sem))
      val storedProperty = new StoredProperty(info)

      def deserializeValues[T](storedProperty: StoredProperty[T]): Unit = {
        val xmlValuesArray = values.pop().toString()
        val xStream = new XStream()
        val rawValuesArray = xStream.fromXML(xmlValuesArray)
        val valuesArray = rawValuesArray.asInstanceOf[Array[(T, Long)]].toList
        storedProperty.values = valuesArray
      }
      deserializeValues(storedProperty)

      properties.push(storedProperty)
    }
  }

  private class EntityBuilder(s: Storage) extends EvHandler {
    val ids   = new mutable.Stack[UUID]()
    val names = new mutable.Stack[String]()
    val properties  = new mutable.Stack[StoredProperty[_]]()

    addEvHandler(Exactly("entity", "id"), OnText(onTextId))
    addEvHandler(Exactly("entity", "name"), OnText(onTextName))
    addEvHandler(Exactly("entity", "property"), new PropertyBuilder(properties))

    def onTextId(t: EvText): Unit = {
      ids.push(UUID.fromString(t.text))
    }

    def onTextName(t: EvText): Unit = {
      names.push(t.text)
    }

    override def onEnd(e: EvElemEnd): Unit = {
      val newEntity = new StoredEntity(ids.pop(), names.pop())
      newEntity.properties = properties.elems
      properties.clear()
      s.entities += newEntity
    }
  }


}
