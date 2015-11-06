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

import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.description.{SVal, SValSet, EntityAspect, TypedSValSet}
import simx.core.ontology._
import simx.core.svaractor.SVarActor

/**
 * Created by
 * martin
 * in September 2015.
 */
trait EntityCreationDSL extends SVarActor with EntityCreationHandling {
  private var toCreate = List[EntityDescription]()
  private var started = false

  object Creates {
    def entity(that: `with`.type) = new IncompleteCreation()
  }

  object Create {
    def entity(that: `with`.type) = new IncompleteCreation()
  }

  protected class IncompleteCreation private[mipro] () {

    class UnNamedCreation(properties: SValSet) {
      def named(name: Symbol)( implicit entityCreationContext : EntityCreationHandling ): Unit = {
        val description = new EntityDescription(
          aspects = List[EntityAspect](),
          name = name,
          path = List[Symbol](),
          annotations = Set[Annotation](),
          additionalProperties = SValSet(properties.values.flatten.map{s => s.asSVal}.toSeq:_*)
        )
        if(started) description.realize() else toCreate ::= description
      }
      def named(name: String): Unit = named(Symbol(name))
    }

    def properties(properties: SValSet) = new UnNamedCreation(properties)
    def property(property: SVal.SValType[_]) = new UnNamedCreation(new SValSet(property))
  }

  case object `with`

  override def startUp(): Unit = {
    super.startUp()
    toCreate.foreach(_.realize())
    toCreate = Nil
    started = true
  }
}