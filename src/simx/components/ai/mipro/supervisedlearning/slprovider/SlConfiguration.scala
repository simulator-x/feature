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

package simx.components.ai.mipro.supervisedlearning.slprovider

import java.io._
import java.nio.charset.StandardCharsets
import java.nio.file.{Paths, Files}
import java.util.UUID

import breeze.linalg.DenseMatrix
import com.thoughtworks.xstream.XStream
import org.encog.ml.data.MLDataPair
import org.encog.ml.data.basic.{BasicMLData, BasicMLDataSet}
import simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork.{NNBinaryData, Weights, Weight, NNData}
import simx.components.ai.mipro.supervisedlearning.util.ConfigurationSavingMode.ConfigurationSavingMode
import simx.components.ai.mipro.supervisedlearning.util.{ConfigurationSavingMode, NeuralNetworkState, SlConstants}
import NeuralNetworkState.NeuralNetworkState

import scala.collection.mutable.ListBuffer
import scala.xml.{NodeSeq, PrettyPrinter, XML, Elem}

/**
 * XMLHelper object for SLConfigurations.
 *
 * @author Thomas Krause
 */
object XmlHelper
{
  def toXml(array: Array[Double]): Elem = {
    <ul>{array.map(i => <li>{i}</li>)}</ul>
  }

  def toXml(list: List[Double]): Elem = toXml(list.toArray)

  def toXml(array: Array[Int]): Elem = toXml(array.map(_.toDouble))

  def toArray(aDataElem: NodeSeq): Array[Double] = {
    var result = aDataElem.text.split("\n").map(_.replace(" ", ""))
    result = result.filterNot(_.isEmpty)
    result.map(_.toDouble)
  }
}
/**
 * Object that encapsules serizalization/deserialization functions for supervised learning configuration objects.
 *
 * @author Thomas Krause
 */
object ConfigSerializationHelper
{
  /**
   * Serialize configuration object into the file system.
   *
   * @param aConfigObj Config to serialize.
   */
  def serialize(aConfigObj: SlConfiguration, aTargetFolder: File): Unit =
  {
    /* Create the configuration folder if it does not exist */
    if (!aTargetFolder.exists) aTargetFolder.mkdir

    val tFileOut: FileOutputStream = new FileOutputStream(aTargetFolder + "/" + aConfigObj.getQualifiedName)
    val tObjOut: ObjectOutputStream = new ObjectOutputStream(tFileOut)

    try
    {
      tObjOut.writeObject(aConfigObj)
    }
    catch
      {
        case tEx: Exception => print("Could not serialize configuration object.")
      }
  }

  def deserializeNNData(aQualifiedName: String, aTargetFolder: File): Option[NNBinaryData] ={
    var tBack: Option[NNBinaryData] = None

    try
    {
      val tFileIn: FileInputStream = new FileInputStream(aTargetFolder + "\\" + aQualifiedName + ".bin")
      val tObjIn: ObjectInputStream = new ObjectInputStream(tFileIn)

      val tConfigObj = tObjIn.readObject

      /* Ensure type safety for parent class, specific child class can be checked later on (see SerializationTest). */
      tConfigObj match
      {
        case tSlConfigObj: NNBinaryData =>
          tBack = Some(tConfigObj.asInstanceOf[NNBinaryData])
        case _ => println("Deserialized object is instance of SlConfiguration")
      }
    }
    catch
      {
        case tEx: Exception => println("[SupervisedLearning] Could not deserialize configuration object.")
      }

    tBack
  }

  /**
   * Deserialize configuration object from file system.
   *
   * @param aQualifiedName Name of the config to deserialize.
   * @return
   */
  def deserialize(aQualifiedName: String, aTargetFolder: File): Option[SlConfiguration] =
  {
    var tBack: Option[SlConfiguration] = None

    try
    {
      val tFileIn: FileInputStream = new FileInputStream(aTargetFolder + "/" + aQualifiedName)
      val tObjIn: ObjectInputStream = new ObjectInputStream(tFileIn)

      val tConfigObj = tObjIn.readObject

      /* Ensure type safety for parent class, specific child class can be checked later on (see SerializationTest). */
      tConfigObj match
      {
        case tSlConfigObj: SlConfiguration =>
          tBack = Some(tConfigObj.asInstanceOf[SlConfiguration])
        case _ => println("Deserialized object is no instance of SlConfiguration")
      }
    }
    catch
      {
        case tEx: Exception => println("Could not deserialize configuration object.")
      }

    tBack
  }

  /**
   * Writes an XML file to the file system.
   *
   * @param aFileName The name of the file to write.
   * @param aXml The Scala XML element to write.
   */
  def writeXml(aFileName: String, aXml: Elem, aTargetFolder: File): Unit =
  {
    val tPrettyPrinter = new PrettyPrinter(120, 2)
    val tPrettyXml = tPrettyPrinter.format(aXml)

    Files.write(Paths.get(aTargetFolder + "/" + aFileName + ".xml"), tPrettyXml.getBytes(StandardCharsets.UTF_8))
    println("[SlConfiguration] Finished Writing File: " + aFileName)
  }

  /**
   * Reads from an XML file and creates an Scala XML element.
   *
   * @param aFileName The name of the file to read.
   * @return A Scala XML element.
   */
  def readXml(aFileName: String, aTargetFolder: File): Option[Elem] =
  {
    var tBack: Option[Elem] = None

    try
    {
      tBack = Some(XML.loadFile(aTargetFolder + "/" + aFileName + ".xml"))
    }
    catch
    {
      case tFileNotFound: FileNotFoundException => tBack = None
      case _: Throwable => println("Any other Exception than FileNotFound occured while reading XML.")
    }

    tBack
  }
}

/**
 * Parent class for all configuration objects.
 *
 * @param aQualifiedName Name of the config, so it can be recognized in the file system if re-used later.
 * @author Thomas Krause
 */
abstract class SlConfiguration(aQualifiedName: String, aSavingMode: ConfigurationSavingMode,
  aTargetFolder: File = new File(SlConstants.CONFIG_SUB_DIR), aId: UUID = UUID.randomUUID()) extends Serializable
{
  protected var iId = aId
  protected val iQualifiedName: String = aQualifiedName
  protected val iTargetFolder: File = aTargetFolder
  protected var iSavingMode: ConfigurationSavingMode = aSavingMode

  if (iTargetFolder.exists() == false)
  {
    iTargetFolder.mkdir()
  }

  /**
   * Returns the configuration's unique identifier.
   *
   * @return
   */
  def getId(): UUID = {iId}

  /**
   * Returns the name of the configuration.
   *
   * @return
   */
  def getQualifiedName: String = {iQualifiedName}

  /**
   * Returns the target folder as File.
   *
   * @return
   */
  def getTargetFolder: File = {iTargetFolder}

  /**
   *
   * @param aSavingMode
   */
  def setSavingMode(aSavingMode: ConfigurationSavingMode): Unit = {iSavingMode = aSavingMode}

  /**
   *
   * @return
   */
  def getSavingMode(): ConfigurationSavingMode = iSavingMode

  /**
   * Calls serialization function for this object instance.
   */
  def updateToFileSystem(): Unit =
  {
    if (iSavingMode == ConfigurationSavingMode.BINARY)
    {
      ConfigSerializationHelper.serialize(this, iTargetFolder)
    }
    else if (iSavingMode == ConfigurationSavingMode.XML)
    {
      ConfigSerializationHelper.writeXml(iQualifiedName, toXml(), iTargetFolder)
    }
    else
    {
      throw new RuntimeException("[SlConfiguration] Wrong or no saving mode specified.")
    }
  }

  /**
   * Tries to deserialize and return an configuration object with the current instance's name.
   *
   * @return
   */
  def restoreFromFileSystem(): Option[SlConfiguration] =
  {
    var tBack: Option[SlConfiguration] = None

    if (iSavingMode == ConfigurationSavingMode.BINARY)
    {
      tBack = ConfigSerializationHelper.deserialize(iQualifiedName, iTargetFolder)
    }
    else if (iSavingMode == ConfigurationSavingMode.XML)
    {
      val tElem: Option[Elem] = ConfigSerializationHelper.readXml(iQualifiedName, iTargetFolder)

      if (tElem.isDefined)
      {
        tBack = Some(fromXml(tElem.get))
      }
    }
    else
    {
      throw new RuntimeException("[SlConfiguration] Wrong or no saving mode specified.")
    }

    tBack
  }

  /**
   * Every child class needs to implent logic in order to restore the serialized object or the XML representation of the
   * object from the file system.
   *
   * @return
   */
  protected def tryRestore() : Boolean

  /**
   * Every child class needs to implement logic for the creation of a XML representation.
   *
   * @return An XML element that can be passed into a parser.
   */
  protected def toXml(): Elem

  /**
   * Every child class needs to implement for the object creation from a XML representation.
   *
   * @return
   */
  protected def fromXml(aXml: Elem): SlConfiguration
}
