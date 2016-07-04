/*
 * Copyright 2016 The SIRIS Project
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

import simplex3d.math.floatx.ConstVec3f
import simx.components.ai.mipro.{SemanticEntityReference, Producer, Start}
import simx.core.entity.description.{SValSet, SVal}
import simx.core.entity.typeconversion.TypeInfo
import simx.core.ontology.types.{OntologySymbol, SemanticSValType}
import simx.core.ontology._
import simx.core.svaractor.semantictrait.base.{Thing, Base}

/**
 * Created by martin on 03/03/2016.
 */

object Decayer {
  def resultPropertyOf[Float,B,X <: Base, S <: Thing, T <: SVal[Float, TypeInfo[Float, Float], X, S]](
    property: SValDescription[Float,B,X,S] with SemanticSValType[T]) =
  {
    val symbol: GroundedSymbol = OntologySymbol.lookup(property.semantics.toSymbol).getOrElse(Symbols.confidence)
    types.Processed.setAnnotations(Set[Annotation](types.Typ(symbol)))
  }
}

/**
 * Applies a time-based decay function to a real number entity property.
 * @param property The property to be processed (e.g. types.Confidence)
 * @param entityWithReal A semantic reference to the entity from which the property shall be processed
 * @param damping The dampening factor of the decay function. Has to be >= 1f.
 * @param epsilon An epsilon value used to determine if the decaying value is equal to zero.
 */
class Decayer[B,X <: Base, S <: Thing, T <: SVal[Float, TypeInfo[Float, Float], X, S]](
  property: SValDescription[Float,B,X,S] with SemanticSValType[T],
  entityWithReal: SemanticEntityReference,
  damping: Float = 500f,
  epsilon: Float = 0.0001f
) extends Producer
{ //that

  def this(
    property: SValDescription[Float,B,X,S] with SemanticSValType[T],
    entityWithReal: SVal.SValType[_]) = this(property, SValSet(entityWithReal))

  private var lastValue = 0f
  private var lastTimestamp = System.currentTimeMillis()
  private var lastConsideredTimestamp = 0L
  private val resultProperty = Decayer.resultPropertyOf(property)

  Requires property types.Confidence from entityWithReal

  Updates the properties of entityWithReal every types.Milliseconds(16L) `with` {
    val now = System.currentTimeMillis()
    val elapsed = if(now == lastTimestamp) 1f else  (now - lastTimestamp).toFloat
    lastTimestamp = now

    lastValue *= (1f + damping) / (elapsed + damping)
    if(lastValue < epsilon) lastValue = 0f

    var newValue = 0f
    try {
      val timestamp = (property of entityWithReal).timestamp
      if(timestamp != lastConsideredTimestamp) {
        lastConsideredTimestamp = timestamp
        newValue = (property of entityWithReal).value
      }
    } catch {
      case _: LocalCopyNotFound => //println("not yet")
    }

    if(newValue > lastValue) lastValue = newValue

    //types.Vector3(ConstVec3f(lastValue, 0f, 0f))
    resultProperty(lastValue)
  }
}
