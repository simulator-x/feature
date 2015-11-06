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

package simx.components.ai.mipro

import simx.core.ontology.types

/**
 * Created by
 * martin
 * in September 2015.
 */
trait NamedActor {
  protected object Is {
    def named(name: String): Unit = {
      _name = Some(name)
    }
    def named(name: Symbol): Unit = {
      _name = Some(name.name)
    }
    def named(name: types.Name): Unit = {
      _name = Some(name.value)
    }
  }

  private[mipro] var _name: Option[String] = None
  def actorName: String = if(_name.isEmpty) getClass.getSimpleName else _name.get
}