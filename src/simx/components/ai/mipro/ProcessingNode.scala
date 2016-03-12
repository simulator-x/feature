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

import simx.core.entity.Entity
import simx.core.entity.description.SVal
import simx.core.ontology.SValDescription
import simx.core.ontology.functions.DefaultSemanticFunctions
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.worldinterface.entity.filter.EntityFilter

/**
 * Created by
 * martin
 * in September 2015.
 */
trait ProcessingNode extends SemanticValueDSL with EventDSL {

  type IncompleteProduction_WithType
  type IncompleteProduction_ToType[DataType,B,X <: Base, S <: Thing]

  implicit var semanticFunctions = new DefaultSemanticFunctions {}
  private[mipro] var onStartUp = List[() => Unit]()

  protected val minTimeBetweenProductionWarningsInMillis: Long = 5000L
  private var lastProductionWarningTimestamp = 0L
  private var numberOfSkippedWarnings = 0

  override protected def startUp(): Unit = {
    super.startUp()
    onStartUp.foreach(_.apply())
  }

  protected def printlnProductionWarning(s: String): Unit = {
    val now = System.currentTimeMillis()
    if(now - lastProductionWarningTimestamp > minTimeBetweenProductionWarningsInMillis) {
      lastProductionWarningTimestamp = now
      println("[warn][" + actorName + "][production] " + s)
      if(numberOfSkippedWarnings > 0) {
        println("\t [warn][Processor][production] Skipped " + numberOfSkippedWarnings +
          " warnings before this one. Use Processor.minTimeBetweenProductionWarningsInMillis to print more warnings.")
      }
      numberOfSkippedWarnings = 0
    } else {
      numberOfSkippedWarnings += 1
    }
  }

  private[mipro] def createIncompleteProduction_With(
    semanticEntityReference: SemanticEntityReference, 
    entityRequestCall: (EntityFilter, Entity => Any) => Unit): IncompleteProduction_WithType

  private[mipro] def createIncompleteProduction_To[DataType,B,X <: Base, S <: Thing] (
    semanticEntityReference: SemanticEntityReference,
    semanticType: SValDescription[DataType,B,X,S],
    entityRequestCall: (EntityFilter, Entity => Any) => Unit ): IncompleteProduction_ToType[DataType,B,X,S]

  protected case object entities
  protected case object properties

  //Updates properties of RightHand with {...}
  //Updates propertiesOf entity describedBy RightHand with {...}
  protected object Updates {
    def the(what: properties.type) = new {
      def of(semanticEntityReference: SemanticEntityReference) = 
        createIncompleteProduction_With(semanticEntityReference, onOneEntityAppearance(_)(_))
      def of(semanticEntityReference: SVal.SValType[_]) =
        createIncompleteProduction_With(
          new SemanticEntityReference(semanticEntityReference),
          onOneEntityAppearance(_)(_))
      def of(what: entity.type) = new {
        def describedBy(semanticEntityReference: SemanticEntityReference) = 
          createIncompleteProduction_With(semanticEntityReference, onOneEntityAppearance(_)(_))
        def describedBy(semanticEntityReference: SVal.SValType[_]) =
          createIncompleteProduction_With(
            new SemanticEntityReference(semanticEntityReference),
            onOneEntityAppearance(_)(_))
      }
      def ofAll(semanticEntityReference: SemanticEntityReference) = 
        createIncompleteProduction_With(semanticEntityReference, handleEntityRegistration(_)(_))
      def ofAll(what: entities.type) = new {
        def describedBy(semanticEntityReference: SemanticEntityReference) = 
          createIncompleteProduction_With(semanticEntityReference, handleEntityRegistration(_)(_))
        def describedBy(semanticEntityReference: SVal.SValType[_]) =
          createIncompleteProduction_With(
            new SemanticEntityReference(semanticEntityReference),
            handleEntityRegistration(_)(_))
      }
    }
  }

  //Sets types.Position of RightHand to {...}
  //Sets types.Position of entity described by RightHand to {...}
  protected object Sets {
    def property[DataType, B, X <: Base, S <: Thing](semanticType: SValDescription[DataType, B, X, S]) = new {
      def of(semanticEntityReference: SemanticEntityReference) =
        createIncompleteProduction_To(semanticEntityReference, semanticType, onOneEntityAppearance(_)(_))
      def of(what: entity.type) = new {
        def describedBy(semanticEntityReference: SemanticEntityReference) =
          createIncompleteProduction_To(semanticEntityReference, semanticType, onOneEntityAppearance(_)(_))
      }
      def ofAll(semanticEntityReference: SemanticEntityReference) =
        createIncompleteProduction_To(semanticEntityReference, semanticType, handleEntityRegistration(_)(_))
      def ofAll(what: entities.type) = new {
        def describedBy(semanticEntityReference: SemanticEntityReference) =
          createIncompleteProduction_To(semanticEntityReference, semanticType, handleEntityRegistration(_)(_))
      }
    }
  }
}
