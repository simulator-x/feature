/*
 * Copyright 2013 The SIRIS Project
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

package simx.components.ai.feature.collection

import simx.core.entity.description.{Providing, SValNotFound, SVal}
import scala.collection.mutable
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/5/13
 * Time: 1:02 PM
 */

class BufferedSValSet(
  accessErrorMsg: (TypeInfo[_, _]) => String =
    (tpe: TypeInfo[_, _]) => "[error][BufferedSValSet] Requested type '" + tpe + "' not in set."
) {

  private[feature] var _rec = false

  private val _data = mutable.HashMap[TypeInfo[_, _], RingBuffer[_]]()

  private[feature] def add[T](buffer : RingBuffer[T])(implicit description: TypeInfo[T, T]) {
    _data.update(description, buffer)
  }

  override def toString: String =
    "BufferedSValSet\n\t" + _data.mkString("\n\t")

  def apply[T](description: TypeInfo[T, T]): RingBuffer[T] =
    _data.get(description).getOrElse(throw new Exception(accessErrorMsg(description))).asInstanceOf[RingBuffer[T]]

  def of[T](description: TypeInfo[T, T]): RingBuffer[T] = apply(description)

  def record = _rec
}