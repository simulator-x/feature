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

package simx.components.ai.mipro.supervisedlearning.helper

import simplex3d.math.floatx.{Vec3f, ConstVec3f}
import simx.core.entity.description.SVal

/**
 * Created by chrisz on 10/06/15.
 */
object Conversion {
  def createMatrix(x: List[Double], xs: List[Double]*) = x ::: xs.flatten.toList

  implicit def constVec3fsValToList(sVal: SVal.SValType[ConstVec3f]): List[Double] = sVal.value


//  implicit def toDenseMat(data: ConstVec3f):DenseMatrix[Double]={
//    DenseMatrix{(data.x.toDouble, data.y.toDouble, data.z.toDouble)}
//  }

  implicit def toList(data: ConstVec3f): List[Double] = {
    data.x.toDouble :: data.y.toDouble :: data.z.toDouble :: Nil
  }

  implicit def toList(data: Vec3f): List[Double] = {
    data.x.toDouble :: data.y.toDouble :: data.z.toDouble :: Nil
  }

  implicit def toList(int: Int): List[Double] = {
    int :: Nil
  }

  implicit def toList(b: Boolean): List[Double] = {
    (if(b) 1.0 else 0.0) :: Nil
  }

  def toPredictionX(x: ConstVec3f, xs: ConstVec3f*) = {
     x ::: xs.flatten.toList
  }

}
