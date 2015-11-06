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

import java.util.Collections

import simx.components.ai.mipro.supervisedlearning.slprovider.XmlHelper
import scala.xml.{Elem, NodeSeq}

/**
 * Created by chris on 24/08/15.
 */
case class Weights(var _data: List[Weight] = Nil){

  def data = _data

  def setData(value: List[Weight])={
    _data = value
  }

  def fromXML(value: NodeSeq) {
    var weights: List[Weight] = Nil
    value.\\("WEIGHT").foreach{node =>
      val vars = XmlHelper.toArray(node)
      weights ::= new Weight(vars(0).toInt, vars(1).toInt, vars(2).toInt, vars(3))
    }
    _data = weights
  }

  private val wCount = 4

  def toXML: Elem =  <WEIGHTS  count={String.valueOf(wCount)}>{data.map(_.toXML)}</WEIGHTS>

  override def equals(weights: Any): Boolean =
  {
    weights match
    {
      case someWeights: Weights =>
        someWeights.data.equals(data)
      case _ => false
    }
  }
}

case class Weight(fromLayer: Int, fromNeuron: Int, toNeuron: Int, value: Double){

  def toXML: Elem = {
    <WEIGHT>
      <ul>
        <li>{fromLayer.toString}</li>
        <li>{fromNeuron.toString}</li>
        <li>{toNeuron.toString}</li>
        <li>{value.toString}</li>
      </ul>
    </WEIGHT>
  }

}
