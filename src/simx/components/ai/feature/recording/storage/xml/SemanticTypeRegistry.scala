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

import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.ontology.{SValDescription, Symbols, types}

/**
 * Created by martin 
 * on 05/08/15.
 */
trait SemanticTypeRegistry {
  protected case class SemanticTypeReference(semantics: String)
  private var semanticTypes = Map[SemanticTypeReference, ConvertibleTrait[_]]()
  private var initialized = false

  //TODO: Resolve and remove
  private def addSpecialSemanticTypes(): Unit = {
    registerSemanticType(types.Identifier as Symbols.trackingTargetId)
    registerSemanticType(types.String as Symbols.uRL)
  }

  private def registerSemanticType(semanticType: ConvertibleTrait[_]): Unit = {
    semanticTypes = semanticTypes.updated(SemanticTypeReference(semanticType.semantics.toString), semanticType)
  }

  private def init(typePackages: List[String] = List("simx.core.ontology.types.package")): Unit = {
    if(!initialized) {
      initialized = true
      val ontoClasses: List[Class[_]] =
        typePackages.flatMap { pkg => Class.forName(pkg).getDeclaredClasses.toList }
      ontoClasses.foreach { c =>
        if (c.getFields.exists(_.getName == "MODULE$")) {
          val semanticType = c.getField("MODULE$").get(c).asInstanceOf[SValDescription[_, _, _, _]]
          registerSemanticType(semanticType)
        }
      }
      addSpecialSemanticTypes()
    }
  }

  protected def lookUpSemanticType(ref: SemanticTypeReference): Option[ConvertibleTrait[_]] = synchronized {
    init()
    semanticTypes.get(ref)
  }
}
