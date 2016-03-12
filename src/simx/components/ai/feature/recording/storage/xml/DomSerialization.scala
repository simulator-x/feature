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

import java.util.UUID

import java.io.{File, ObjectInputStream, FileInputStream, FileNotFoundException, OutputStreamWriter, BufferedWriter, FileOutputStream}
import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.converters.extended.EncodedByteArrayConverter
import simplex3d.math.floatx.{ConstVec3f, Vec3f, ConstMat4f}
import simx.components.ai.feature.recording.storage.{StorageMetaData, StoredProperty, StoredEntity, Storage}
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.helper.{chirality, IO}
import simx.core.ontology.{SValDescription, types}
import simx.core.svaractor.semantictrait.base.GroundedSymbolFeatures

import scala.xml.{XML, Unparsed, Node, Elem}
import scala.reflect.runtime.universe._


/**
 * Created by martin 
 * on 05/08/15.
 */
class DomSerialization extends SemanticTypeRegistry{

  var print_to_console = true

  def fromXml(storageFile: File): Storage = {
    if(print_to_console)
      println(Console.MAGENTA + "Reading " + storageFile.getAbsolutePath + Console.RESET)

    var before = System.currentTimeMillis()
    val storageXml = XML.loadFile(storageFile)
    val storage = new Storage
    if(print_to_console)
      println(Console.GREEN + "Finished reading file (" + (System.currentTimeMillis() - before) + "ms)" + Console.RESET)

    if(print_to_console)
      println(Console.MAGENTA + "Parsing " + storageFile.getName + Console.RESET)

    before = System.currentTimeMillis()
    storage.metaData ::=
      StorageMetaData(Some(storageFile), (storageXml \ "companionVideoStartTime").headOption.map{_.text.toLong})
    parseEntities(storageXml, storage)

    if(print_to_console)
      println(Console.GREEN + "Finished parsing file (" + (System.currentTimeMillis() - before) + "ms)" + Console.RESET)

    storage
  }

  /**
   * Converts an entity recording stored as binary file to a XML-based format.
   * The conversion result is saved in the same folder and with the same file name as binaryStorageFile.
   * @param prettyPrint Format final XML nicely.
   *                    Requires a high amount of heap space for large files.
   *                    Use JVM option -Xmx to increase the heap space (e.g. '-Xmx4g').
   */
  def convertToXml(binaryStorageFile: File, prettyPrint: Boolean = false): Unit = {
    val ooi = new ObjectInputStream(new FileInputStream(binaryStorageFile))
    val storage = ooi.readObject().asInstanceOf[Storage]

    fixSemantics(storage)

    val nrOfEntities = storage.entities.size
    val nrOfValueChanges  = storage.entities.toList.map(_.properties.map{_.values.size}).flatten.sum
    val timestamps        = storage.entities.flatMap(_.properties.map{_.values.map{_._2}}).flatten.toList.sortWith(_ < _)
    val duration = timestamps.last - timestamps.head

    if(print_to_console) {
      println("Loaded storage file with " + nrOfEntities + " entities, " + nrOfValueChanges + " value changes, and a duration of " + duration + " ms.")
      println(storage.entities.map {
        _.name
      }.mkString("[", ", ", "]"))
    }
    saveToFile(storage, IO.changeExtension(binaryStorageFile, "xml"), prettyPrint)
  }

  def saveToFile(storage: Storage, destination: File, prettyPrint: Boolean = false): Unit = {
    if(print_to_console)
      println(Console.MAGENTA + "Starting xml serialization to " + destination.getAbsolutePath + Console.RESET)

    val before = System.currentTimeMillis()
    val xml = toXML(storage)
    save(xml, destination, prettyPrint, addXmlDeclaration = true)
    if(print_to_console)
      println(Console.GREEN + "Finished xml serialization (" + (System.currentTimeMillis() - before) + "ms)" + Console.RESET)
  }

  def recordingStartFromXml(storageFile: File): Option[Long] =
    (XML.loadFile(storageFile) \ "companionVideoStartTime").headOption.map{_.text.toLong}

  private def parseEntities(storageElem: Elem, storage: Storage): Unit = {
    (storageElem \ "entity").foreach{ entityElem =>
      //val before = System.currentTimeMillis()
      val id = UUID.fromString((entityElem \ "id").head.text)
      val name = (entityElem \ "name").head.text
      //println(Console.MAGENTA + "\tParsing entity " + name + Console.RESET)
      val storedEntity = new StoredEntity(id, name)
      parseProperties(entityElem, storedEntity)
      storage.entities += storedEntity
      //println(Console.GREEN + "\tFinished parsing entity (" + (System.currentTimeMillis() - before) + "ms)" + Console.RESET)
    }
  }

  private def parseProperties(entityElem: Node, storedEntity: StoredEntity): Unit = {
    (entityElem \ "property").foreach{ propertyElem =>
      val semantics = (propertyElem \ "semantics").head.text
      val info = lookUpSemanticType(SemanticTypeReference(semantics)).getOrElse(throw new Exception("Failed to look up semantic type " + semantics))
      val storedProperty = new StoredProperty(info)
      parseValues(propertyElem, storedProperty)
      storedEntity.properties ::= storedProperty
    }
  }

  private def parseValues[T](propertyElem: Node, storedProperty: StoredProperty[T]): Unit = {
    val xStream = new XStream()
    val rawValuesArray = ((propertyElem \ "values").head \ "scala.Tuple2-array").toString()
    val values = xStream.fromXML(rawValuesArray)
    storedProperty.values = values.asInstanceOf[Array[(T, Long)]].toList
  }

  private def toXML[T](property: StoredProperty[T]): Elem = {
    val xStream = new XStream()
    //val baseIri = property.info.ontoLink.getOrElse("")
    val values = xStream.toXML(property.values.toArray)
    val semantics = property.info.semantics.toString
    <property>
      <semantics>{semantics}</semantics>
      <values>{Unparsed(values.replace("\n", "\n        "))}</values>
    </property>
  }

  private def toXML(entity: StoredEntity): Elem = {
    <entity>
      <id>{entity.id.toString}</id>
      <name>{entity.name}</name>
      {entity.properties.map{p => toXML(p)}}
    </entity>
  }

  def toXML(storage: Storage): Elem = {
    <entityRecording>
      {if(storage.metaData.nonEmpty)
      <companionVideoStartTime>{storage.metaData.head.recordingStart.get.toString}</companionVideoStartTime>}
      {storage.entities.map(toXML)}
    </entityRecording>
  }

  private def pretty(xml: Elem) = {
    val prettyPrinter = new scala.xml.PrettyPrinter(80, 2)
    prettyPrinter.format(xml)
  }

  private def save(xml: Elem, file: File, prettyPrint: Boolean = false, addXmlDeclaration: Boolean = false): Unit = {
    var xmlString = if(prettyPrint) pretty(xml) else xml.toString()
    if(addXmlDeclaration)
      xmlString = """<?xml version='1.0' encoding='UTF-8'?>""" + "\n" + xmlString

    val out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(file), "UTF-8"))
    try {out.write(xmlString)} finally {out.close()}
  }

  private def getOuter(o: Any) = Option[Any] {
    val outerField = o.getClass.getDeclaredFields.toList.find(_.getName == "$outer")
    outerField.map{f => f.setAccessible(true); f.get(o)}
  }

  private def flatten(o: Option[Any]): Option[Any] = o match {
    case Some(thing @ Some(_)) => flatten(thing)
    case flat => flat
  }

  //TODO Resolve undelying bug and remove method
  private def fixSemantics(storage: Storage): Unit = {
    storage.entities.foreach{e =>
      e.properties.foreach{ p =>
        def correct[T](prop: StoredProperty[T]): Unit = {
          prop.info = types.Chirality.asInstanceOf[ConvertibleTrait[T]]
        }
        if(p.values.headOption.exists{x => x._1.isInstanceOf[chirality.Chirality]}) correct(p)
      }
      e.properties.foreach{ p =>
        def correct[T](prop: StoredProperty[T]): Unit = {
          prop.info = types.Normal.asInstanceOf[ConvertibleTrait[T]]
        }
        def isNormal(groundedSymbol: Any) = {
          val outer = flatten(getOuter(groundedSymbol)).map{_.asInstanceOf[GroundedSymbolFeatures]}
          outer.exists(_.toString == "normal")
        }
        if(isNormal(p.info.asInstanceOf[SValDescription[_,_,_,_]].valueDescription.groundedSymbol)) correct(p)
      }
    }
  }
}

