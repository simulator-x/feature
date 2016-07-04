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

import java.util.UUID

import simx.core.entity.Entity
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.description.SVal._
import simx.core.entity.description._
import simx.core.entity.typeconversion.TypeInfo
import simx.core.helper.EntityObservation
import simx.core.ontology._
import simx.core.svaractor.TimedRingBuffer
import simx.core.svaractor.TimedRingBuffer.At
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.svaractor.unifiedaccess.{EntityUpdateHandling, StateParticleInfo}
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.worldinterface.entity.filter.EntityFilter
import simx.core.worldinterface.eventhandling.EventHandler

/**
 * Created by
 * martin
 * in June 2015.
 */
abstract class Processor extends ProcessingNode
  with WorldInterfaceHandling
  with EntityUpdateHandling with EntityObservation with EntityCreationHandling
  with EventHandler
{
  featureActor =>

  implicit val sync = SyncTime(0L)
  private var syncTimes = Map[UUID, Long]()

  type IncompleteProduction_WithType = IncompleteProduction_With
  type IncompleteProduction_ToType[DataType,B,X <: Base, S <: Thing] = IncompleteProduction_To[DataType,B,X,S]

  private var productions = Map[Entity, List[() => SVal.SValType[_]]]()

  protected class IncompleteProduction_With private[mipro] (
   semanticEntityReference: SemanticEntityReference,
   entityRequestCall: (EntityFilter, Entity => Any) => Unit )
  {
    def `with`(production: => SVal.SValType[_]) =
    {
      addRule(semanticEntityReference, entityRequestCall, () => production)
    }
  }

  protected class IncompleteProduction_To[DataType,B,X <: Base, S <: Thing] private[mipro] (
    semanticEntityReference: SemanticEntityReference,
    semanticType: SValDescription[DataType,B,X,S],
    entityRequestCall: (EntityFilter, Entity => Any) => Unit )
  {
    def to(production: => DataType): Unit  = {
      addRule(semanticEntityReference, entityRequestCall, () => semanticType(production))
    }
  }

  private[mipro] def createIncompleteProduction_To[DataType, B, X <: Base, S <: Thing](
    semanticEntityReference: SemanticEntityReference,
    semanticType: SValDescription[DataType, B, X, S],
    entityRequestCall: (EntityFilter, (Entity) => Any) => Unit): IncompleteProduction_To[DataType, B, X, S] =
  {
    new IncompleteProduction_To(semanticEntityReference, semanticType, entityRequestCall)
  }

  private[mipro] def createIncompleteProduction_With(
    semanticEntityReference: SemanticEntityReference,
    entityRequestCall: (EntityFilter, (Entity) => Any) => Unit): IncompleteProduction_With =
  {
    new IncompleteProduction_With(semanticEntityReference, entityRequestCall)
  }

  private def addRule(
    semanticEntityReference: SemanticEntityReference,
    entityRequestCall: (EntityFilter, Entity => Any) => Unit,
    production: () => SVal.SValType[_]): Unit =
  {
    onStartUp ::= {() => entityRequestCall(semanticEntityReference.toFilter, e => {
      val currentProductions: List[() => SValType[_]] = productions.getOrElse(e, Nil)
      productions = productions.updated(e, production :: currentProductions)
    })}
  }

  private var _allRequirementsUnderObservation = false
  private def allRequirementsUnderObservation = {
    if(!_allRequirementsUnderObservation) {
      _allRequirementsUnderObservation =
        requirements.map(_.size).sum == syncTimes.size
    }
    _allRequirementsUnderObservation
  }

  override def onStartOfObservation(e: Entity, requirementInfo: StateParticleInfo[_]): Unit = {
    //println("observing " + info.svar)
    //println(e.id, info.svar.id)
    syncTimes = syncTimes.updated(requirementInfo.svar.id, 0L)
  }

  override def onNewRequirementValue(
    e: Entity,
    requirementInfo: StateParticleInfo[_],
    timestamp: simx.core.svaractor.TimedRingBuffer.Time): Unit =
  {
    //println(localData.keySet.size)
    //println(storage)
    syncTimes = syncTimes.updated(requirementInfo.svar.id, timestamp.timeInMillis)

    //Oldest timestamp amongst the timestamps of the newest value for each requirement (aka. stamp ba di stamp time)
    val oldestTimestamp = syncTimes.values.min

    //val atLeastOneValueOfEachPropertyArrived =
    // syncTimes.size == localData.values.toSeq.flatMap(_.values.toSeq.map(_.size)).sum
    if(allRequirementsUnderObservation && oldestTimestamp > sync.t0) {
      sync.t0 = oldestTimestamp
      //      println(sync.t0, syncTimes)
      //sync.t0 = timestamp.timeInMillis
      checkProductions()
    }
  }

  protected final val NoResult = types.Result(Symbols.none).asAny

  private def checkProductions(): Unit = {
    productions.foreach{tuple =>
      val e = tuple._1
      val ps = tuple._2
      ps.foreach{p =>
        //println("Setting " + e.getSimpleName)
        try {
          val res = p.apply()
          res match {
            case sVal: types.Result if sVal.value == Symbols.none =>
              //println("[info][Processor] No result, skipping to set entity property.")
            case _ =>
              e.set(res, At(sync.t0), TimedRingBuffer.UnbufferedTimeStoring){(_ : e.SelfType) => {}}
          }
        } catch {
          case e: LocalCopyNotFound =>
            printlnProductionWarning(e.msg)
          case e: SValNotFound =>
            printlnProductionWarning("SVal not found:" + e.sVarIdentifier)
          case e: SValHistoryException =>
            printlnProductionWarning(e.msg)
          case e: InterpolationException =>
            printlnProductionWarning(e.message)
        }
      }
    }
  }

  override protected def removeFromLocalRep(e: Entity): Unit = {}
}