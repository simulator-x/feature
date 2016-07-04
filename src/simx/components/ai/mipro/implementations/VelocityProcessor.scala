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

package simx.components.ai.mipro.implementations

import simx.components.ai.mipro._
import simx.core.entity.description.{SValSet, SVal}
import simx.core.ontology.functions.Interpolators
import simx.core.ontology.{SValDescription, Symbols, types}
import simx.core.svaractor.semantictrait.base.{Base, Thing}

/**
 * 
 * Created by martin fischbach
 * on 6/11/2015.
 */
class VelocityProcessor(
  entityWithPosition: SemanticEntityReference,
  deltaT: types.Milliseconds = types.Milliseconds(60L)) extends Processor with Interpolators
{
  private val deltaTinSec = deltaT.value.toFloat / 1000f

  Requires property types.Position from entityWithPosition

  Updates the properties of entity describedBy entityWithPosition `with` {
    val deltaP = (types.Position of entityWithPosition at types.Milliseconds(0)).value - 
                 (types.Position of entityWithPosition at deltaT).value
    types.Velocity(deltaP / deltaTinSec)
  }
}

class AccelerationProcessor(
   entityWithVelocity: SemanticEntityReference,
   deltaT: types.Milliseconds = types.Milliseconds(60L)) extends Processor with Interpolators
{
  private val deltaTinSec = deltaT.value.toFloat / 1000f

  Requires property types.Velocity from entityWithVelocity

  Updates the properties of entity describedBy entityWithVelocity `with` {
    val deltaV = (types.Velocity of entityWithVelocity at types.Milliseconds(0)).value -
                 (types.Velocity of entityWithVelocity at deltaT).value
    types.Acceleration(deltaV / deltaTinSec)
  }
}

class RelativePositionProcessor(
  entityWithPosition: SemanticEntityReference,
  entityWithTransformation: SemanticEntityReference
  ) extends Processor with EntityCreationDSL
{

  def this(entityWithPosition: SemanticEntityReference, entityWithTransformation: SVal.SValType[_]) =
    this(entityWithPosition, SValSet(entityWithTransformation))

  val entityWithRelativePosition = RelativeProcessor.semanticEntityReferenceFor(entityWithPosition)

  //Is named ("RelativeProcessor" + entityWithPosition.toShortString)
  Creates entity `with` properties entityWithRelativePosition named (entityWithPosition.toShortString + " relative")

  Requires property types.Position       from entity describedBy entityWithPosition
  Requires property types.Transformation from entityWithTransformation

  Updates the properties of entity describedBy entityWithRelativePosition `with` {
    (types.Position of entityWithPosition) relativeTo (types.Transformation of entityWithTransformation)
  }
}

object RelativeProcessor {
  /**
   *  Returns a [[SemanticEntityReference]] that can be used to
   *  reference the entity created by a [[RelativePositionProcessor]].
   *  @param entityWithPosition The [[SemanticEntityReference]] that was passed to
   *                            the respective [[RelativePositionProcessor]].
   */
  def semanticEntityReferenceFor(entityWithPosition: SemanticEntityReference): SemanticEntityReference = {
    def addAnnotationToSValDescription[T, B, X <: Base, S <: Thing](
      semanticType: SValDescription[T,B,X,S]): SemanticEntityReference =
    {
      val entityTypeValue = entityWithPosition.firstValueFor(semanticType)
      //Copy the mutable parameter entityWithPosition to not alter it
      val entityWithRelativePosition = new SemanticEntityReference(entityWithPosition)
      entityWithRelativePosition.remove(semanticType.sVarIdentifier)
      val newEntityType = semanticType.withAnnotations(types.Typ(Symbols.relative))(entityTypeValue)
      entityWithRelativePosition.add(newEntityType)
      entityWithRelativePosition
    }

    if(entityWithPosition.contains(types.EntityType))
    //Add annotation to EntityType property
      addAnnotationToSValDescription(types.EntityType)
    else
    //Add annotation to a random property
      addAnnotationToSValDescription(
        //All properties as ordered sequence (list)
        entityWithPosition.values.flatten.toList.
          //Sort to make result repeatable
          sortWith(_.typedSemantics.semantics.toString < _.typedSemantics.semantics.toString ).
          //Take first in sorted list
          head.
          //Convert underlying semantic type
          asSVal.typedSemantics.asConvertibleTrait.asInstanceOf[SValDescription[_,_,_<: Base,_<:Thing]])
  }
}