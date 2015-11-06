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

package simx.components.ai.mipro.supervisedlearning.slprovider.neuralnetwork

import java.io.{ObjectOutputStream, File, FileOutputStream}
import java.util

import org.encog.ml.data.basic.{BasicMLData, BasicMLDataSet}
import simx.components.ai.mipro.supervisedlearning.slprovider.XmlHelper
import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, NodeSeq}

class NNData(_dataSet: BasicMLDataSet) extends Serializable {

  def this() = this(new BasicMLDataSet())

  var dataSet = _dataSet

  def addDataSet(x: Array[Double], y: Array[Double]): Unit ={
      dataSet.add(new BasicMLData(x), new BasicMLData(y))
  }

  override def toString: String = {
    var result = ""
    for(i <- 0 until dataSet.getData.size()){
      val pair = dataSet.getData.get(i)
        result += util.Arrays.toString(pair.getInputArray) + " -> " + util.Arrays.toString(pair.getIdealArray) + "\n"
    }
    result
  }

  def fromXML(value: NodeSeq) = {
    val result: BasicMLDataSet = new BasicMLDataSet()
    if (value.nonEmpty){
      val tXMatrixElem = value \\ "X_MATRIX"
      val tXCount = tXMatrixElem.\@("count")

      val tYMatrixElem = value \\ "Y_MATRIX"
      val tYCount = tYMatrixElem.\@("count")

      val xSize = tXCount.toInt
      val ySize = tYCount.toInt
      val x = XmlHelper.toArray(tXMatrixElem)
      val y = XmlHelper.toArray(tYMatrixElem)
      var yCounter: Int = 0

      if (xSize > 0)
      {
        for (i <- x.indices by xSize)
        {
          result.add(new BasicMLData(x.slice(i, i + xSize)), new BasicMLData(y.slice(yCounter, yCounter + ySize)))
          yCounter += ySize
        }
      }
    }
    dataSet = result
  }

  def toXML = {
    val (xCount, x, yCount, y) = getXMLRep
    <DATA>
      <X_MATRIX count={String.valueOf(xCount)}>
        {XmlHelper.toXml(x)}
      </X_MATRIX>
      <Y_MATRIX count={String.valueOf(yCount)}>
        {XmlHelper.toXml(y)}
      </Y_MATRIX>
    </DATA>
  }

  def toBinary(aFileName: String, aTargetFolder: File) {
    val (xCount, x, yCount, y) = getXMLRep
    val fileOut = new FileOutputStream(aTargetFolder + "/" + aFileName + ".bin")
    val out = new ObjectOutputStream(fileOut)
    out.writeObject(NNBinaryData(xCount, x, yCount, y))
    out.close()
    fileOut.close()
    println("[SlConfiguration] Finished Writing File: " +  aFileName)
  }

  def fromBinary(nNBinaryData: NNBinaryData) = {
    val result: BasicMLDataSet = new BasicMLDataSet()

    val xSize = nNBinaryData.xCount
    val ySize = nNBinaryData.yCount
    val x =  nNBinaryData.x.toArray
    val y =  nNBinaryData.y.toArray
    var yCounter: Int = 0

    if (xSize > 0)
    {
      for (i <- x.indices by xSize)
      {
        result.add(new BasicMLData(x.slice(i, i + xSize)), new BasicMLData(y.slice(yCounter, yCounter + ySize)))
        yCounter += ySize
      }
    }

    dataSet = result
  }

  private def getXMLRep ={
    val xSize = dataSet.getInputSize
    val ySize = dataSet.getIdealSize
    val data = dataSet.getData

    val x: ListBuffer[Double] = new ListBuffer[Double]()
    val y: ListBuffer[Double] = new ListBuffer[Double]()

    for(i <- 0 until data.size()){
      val curData = data.get(i)
      x.appendAll(curData.getInputArray)
      y.appendAll(curData.getIdealArray)
    }

    (xSize, x.toList, ySize, y.toList)
  }

  def getSize(): Int = dataSet.size()
}


class TestData(_dataSet: BasicMLDataSet) extends NNData {

  def this() = this(new BasicMLDataSet())

  override def fromXML(value: NodeSeq) = {
    val elem = value \\ "TESTING"
    super.fromXML(elem)
  }

  override  def fromBinary(nNBinaryData: NNBinaryData) ={
    super.fromBinary(nNBinaryData)
  }

  override def toXML = {
    <TESTING>
      {super.toXML}
    </TESTING>
  }
}

class TrainData(_dataSet: BasicMLDataSet) extends NNData {

  def this() = this(new BasicMLDataSet())

  override def fromXML(value: NodeSeq) = {
    val elem = value \\ "TRAINING"
    super.fromXML(elem)
  }

  override  def fromBinary(nNBinaryData: NNBinaryData) ={
    super.fromBinary(nNBinaryData)
  }

  override def toXML = {
    <TRAINING>
      {super.toXML}
    </TRAINING>
  }
}
