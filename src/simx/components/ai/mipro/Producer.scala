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
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.description._
import simx.core.entity.typeconversion.TypeInfo
import simx.core.helper.EntityObservation
import simx.core.ontology._
import simx.core.svaractor.TimedRingBuffer
import simx.core.svaractor.TimedRingBuffer.{At, Time}
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.svaractor.unifiedaccess.{EntityUpdateHandling, StateParticleInfo}
import simx.core.worldinterface.WorldInterfaceHandling
import simx.core.worldinterface.entity.filter.EntityFilter
import simx.core.worldinterface.eventhandling.EventHandler

/**
 * Created by
 * martin
 * in September 2015.
 */
abstract class Producer extends ProcessingNode
with WorldInterfaceHandling
with EntityUpdateHandling with EntityObservation with EntityCreationHandling
with EventHandler {
  producerSelf =>

  type IncompleteProduction_WithType = IncompleteProduction_With
  type IncompleteProduction_ToType[DataType,B,X <: Base, S <: Thing] = IncompleteProduction_To[DataType,B,X,S]

  private var startTime = 0L
  private var deltaT = 1L

  override protected def startUp(): Unit = {
    super.startUp()
    startTime = System.currentTimeMillis()
  }

  protected class IncompleteProduction_With private[mipro] (
    semanticEntityReference: SemanticEntityReference,
    entityRequestCall: (EntityFilter, Entity => Any) => Unit )
  {
    def every(period: types.Milliseconds) = new {
      def `with`[DataType, B, X <: Base, S <: Thing](
        production: => SVal[DataType, TypeInfo[DataType, DataType], X, S]) =
      {
        addRule(semanticEntityReference, entityRequestCall, period, () => production)
      }
    }
  }

  protected class IncompleteProduction_To[DataType,B,X <: Base, S <: Thing] private[mipro] (
    semanticEntityReference: SemanticEntityReference,
    semanticType: SValDescription[DataType,B,X,S],                                                
    entityRequestCall: (EntityFilter, Entity => Any) => Unit )
  {
    def every(period: types.Milliseconds) = new {
      def to(production: => DataType): Unit  = {
        addRule(semanticEntityReference, entityRequestCall, period, () => semanticType(production))
      }
    }
  }

  private[mipro] def createIncompleteProduction_To[DataType, B, X <: Base, S <: Thing](
    semanticEntityReference: SemanticEntityReference,
    semanticType: SValDescription[DataType, B, X, S],
    entityRequestCall: (EntityFilter, (Entity) => Any) => Unit): IncompleteProduction_To[DataType, B, X, S] =
  {
    new IncompleteProduction_ToType(semanticEntityReference, semanticType, entityRequestCall)
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
    period: types.Milliseconds,
    production: () => SVal.SValType[_]): Unit =
  {
    onStartUp ::= {() => entityRequestCall(semanticEntityReference.toFilter, e => {
      setUp(RepetitiveProductionRule(e, production, period.value))
    })}
  }

  private case class RepetitiveProductionRule(
    targetEntity: Entity,
    production: () => SVal.SValType[_],
    framePeriodInMillis: Long)
  {
    def performProduction(): Unit = {
      try {
        targetEntity.set(
          production.apply(),
          At(System.currentTimeMillis()),
          TimedRingBuffer.UnbufferedTimeStoring)
        {
          (_ : targetEntity.SelfType) => {}
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

  protected object Context {
    def elapsedTime = types.Milliseconds(System.currentTimeMillis() - startTime)
    def deltaT = types.Milliseconds(producerSelf.deltaT)
  }

  private def setUp(rule: RepetitiveProductionRule): Unit = {
    var lastExecutionStartingTimeInMillis = 0L

    def executeAndScheduleNextCall(): Unit = {
      val currentExecutionStartingTimeInMillis = System.currentTimeMillis
      deltaT =
        if(lastExecutionStartingTimeInMillis == 0) rule.framePeriodInMillis
        else currentExecutionStartingTimeInMillis - lastExecutionStartingTimeInMillis
      lastExecutionStartingTimeInMillis = currentExecutionStartingTimeInMillis

      rule.performProduction()
      addJobIn(rule.framePeriodInMillis){executeAndScheduleNextCall()}
    }

    addJobIn(rule.framePeriodInMillis){executeAndScheduleNextCall()}
  }

  override protected def removeFromLocalRep(e: Entity): Unit = {}

  override def onNewRequirementValue(requirementInfo: StateParticleInfo[_], timestamp: Time): Unit = {}

  override def onStartOfObservation(requirementInfo: StateParticleInfo[_]): Unit = {}
}
