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

import simplex3d.math.floatx.ConstVec3f
import simx.core.entity.Entity
import simx.core.entity.description.SVal._
import simx.core.entity.description.{SVal, SValHistory, SValNotFound, SValSet}
import simx.core.entity.typeconversion.{Converter, ConvertibleTrait, SemanticTypeSet, TypeInfo}
import simx.core.helper.EntityObservation
import simx.core.ontology.{types, SValDescription}
import simx.core.ontology.types.{OntologySymbol, SemanticSValType}
import simx.core.svaractor.SVarActor
import simx.core.svaractor.semantictrait.base.{Base, Thing}
import simx.core.svaractor.unifiedaccess.StateParticleInfo
import simx.core.worldinterface.WorldInterfaceHandling


/**
 * Created by
 * martin
 * in September 2015.
 */
trait SemanticValueDSL extends SVarActor with NamedActor with WorldInterfaceHandling with EntityObservation {

  object Requires {
    def property(requiredProperty: ConvertibleTrait[_]) =
      new IncompleteSemanticEntityReference(SemanticTypeSet.toSemanticTypeSet(requiredProperty))
    def properties(requiredProperties: SemanticTypeSet) =
      new IncompleteSemanticEntityReference(requiredProperties)
  }

  //protected case object entities
  protected case object properties

  object Update {
    def the(what: properties.type) = new {
      def of(semanticEntityReference: SemanticEntityReference) = new IncompleteSetOperation(semanticEntityReference)
      def of(semanticEntityReference: SValType[_]): IncompleteSetOperation =
        of(new SemanticEntityReference(semanticEntityReference))
    }
  }

  class IncompleteSetOperation(semanticEntityReference: SemanticEntityReference) {
    def `with`(value: SVal.SValType[_]): Unit = {

      if(localEntityReferences.contains(semanticEntityReference)) {
        localEntityReferences(semanticEntityReference).set(value)
      } else {

        if(!bufferedEntitySetOperations.contains(semanticEntityReference)) {

          if(_name.isDefined) {
            println("[info][" + actorName + "] Requesting entity described by '"
              + semanticEntityReference.toShortString + "' for set operation." )
          }

          onOneEntityAppearance(semanticEntityReference.toFilter) { e =>

            if(_name.isDefined) {
              println("[info][" + actorName + "] Received entity described by '"
                + semanticEntityReference.toShortString + "' for set operation. Applying " +
                bufferedEntitySetOperations.getOrElse(semanticEntityReference, Nil).size + " buffered set operation(s) now."
              )
            }

            localEntityReferences = localEntityReferences.updated(semanticEntityReference, e)
            bufferedEntitySetOperations.getOrElse(semanticEntityReference, Nil).reverse.foreach(e.set(_))
            bufferedEntitySetOperations -= semanticEntityReference
          }
        }

        val buffer = bufferedEntitySetOperations.getOrElse(semanticEntityReference, Nil)
        bufferedEntitySetOperations =
          bufferedEntitySetOperations.updated(semanticEntityReference, value :: buffer)

        if(_name.isDefined) {
          println("[info][" + actorName + "] Buffering set operation for entity described by '"
            + semanticEntityReference.toShortString + "'. " +
            bufferedEntitySetOperations.getOrElse(semanticEntityReference, Nil).size + " operation(s) is/are now buffered in total." )
        }

      }
    }
  }

  case class LocalCopyNotFound(msg: String) extends Exception(msg)

  protected implicit def
  toLocalCopyRequest[DataType,B,X <: Base, S <: Thing, T <: SVal[DataType, TypeInfo[DataType, DataType], X, S]](
    property: SValDescription[DataType,B,X,S] with SemanticSValType[T]): Object {
        def of(semanticEntityReference: SValType[_]): T with SValHistory[DataType, S, T]
        def of(semanticEntityReference: SemanticEntityReference): T with SValHistory[DataType, S, T]
    } = new
  {
    def of(semanticEntityReference: SemanticEntityReference): T with SValHistory[DataType, S, T] = {
      try {
        val sVal: SValType[DataType] = localData(semanticEntityReference).firstSValFor(property)
        property.apply(sVal.value, sVal.getTimeStamp, sVal.getHistory)
      } catch {
        case e: SValNotFound =>
          throw LocalCopyNotFound("Could not find a local copy of property" +
                                  property.semantics.toString +
                                  " from entity described by " + semanticEntityReference.toString())
        case e: NoSuchElementException =>
          throw LocalCopyNotFound("Could not find any local copy of any property" +
                                  " from entity described by " + semanticEntityReference.toString())
      }
    }

    def of(semanticEntityReference: SVal.SValType[_]): T with SValHistory[DataType, S, T] =
      of(new SemanticEntityReference(semanticEntityReference))
  }

  protected def requirements = _requirements
  private var _requirements = List[ConstraintSemanticEntityReference]()
  //semantic entity description -> local copies of required properties (SValSet)
  protected var localData = Map[SemanticEntityReference, SValSet]()
  //semantic entity description -> local reference to entity
  protected var localEntityReferences = Map[SemanticEntityReference, Entity]()
  private   var bufferedEntitySetOperations = Map[SemanticEntityReference, List[SVal.SValType[_]]]()

  //TODO CleanUp



  protected def allDataSorted: List[(SemanticEntityReference, ConvertibleTrait[_])] = {
    val sortedSemanticEntityReferences = localData.keys.toList.sortWith( (setA, setB) => {
      setA.values.flatten.toList.sortWith(_.toString < _.toString).mkString("") <
      setB.values.flatten.toList.sortWith(_.toString < _.toString).mkString("")
    })

    sortedSemanticEntityReferences.flatMap{ ref =>
      localData(ref).values.flatten.toList.sortWith(_.typedSemantics.toString < _.typedSemantics.toString).map{
        ref -> _.typedSemantics.asConvertibleTrait
      }
    }
  }
  import simx.core.ontology.types



  def test(): Unit = {
    //TODO get allDataSorted only once
    val giefff = allDataSorted

    giefff.foreach{g =>
      val v = localData(g._1).firstSValFor(g._2)
      val magic: List[Float] = v.as(types.Reals).toList
    }


  }
  //TODO CleanUp

  class IncompleteSemanticEntityReference(requiredProperties: SemanticTypeSet) {
    def from(semanticEntityReference: SemanticEntityReference): Unit =
      addRequirement(semanticEntityReference, requiredProperties)

    def from(semanticEntityReference: SVal.SValType[_]): Unit =
      addRequirement(new SemanticEntityReference(semanticEntityReference), requiredProperties)

    def fromAll(semanticEntityReferences: Set[SemanticEntityReference]): Unit = {
      semanticEntityReferences.foreach(from)
    }

    def fromAll(what: entities.type): Unit = {
      def describedBy(semanticEntityReferences: Set[SemanticEntityReference]): Unit = {
        semanticEntityReferences.foreach(from)
      }
    }

    def from(what: entity.type) = new {
      def describedBy(semanticEntityReference: SemanticEntityReference): Unit =
        addRequirement(semanticEntityReference, requiredProperties)

      def describedBy(semanticEntityReference: SVal.SValType[_]): Unit =
        addRequirement(new SemanticEntityReference(semanticEntityReference), requiredProperties)
    }
  }

  private def addRequirement(
    semanticEntityReference: SemanticEntityReference,
    requiredProperties: SemanticTypeSet): Unit =
  {
    _requirements ::= ConstraintSemanticEntityReference(semanticEntityReference, requiredProperties)
  }

  protected case object entity
  protected case object entities
  
  protected case class ConstraintSemanticEntityReference(
    semanticEntityReference: SemanticEntityReference, constraints: SemanticTypeSet)
  {
    def matches[U](info: StateParticleInfo[U]): Boolean = {
      if(constraints.isEmpty) true
      else constraints.exists{pf => info.matches(pf)}
    }

    def size = constraints.size
  }

  override protected def startUp(): Unit = {
    super.startUp()
    _requirements.foreach( requirement => {
      onOneEntityAppearance(requirement.semanticEntityReference.toFilter) { e =>
        if(_name.isDefined) {println("[info][" + actorName + "] Got entity " + e.getSimpleName)}
        val storage = new SValSet()
        localEntityReferences = localEntityReferences.updated(requirement.semanticEntityReference, e)
        localData = localData.updated(requirement.semanticEntityReference, storage)
        onEntityUpdate(e){
          case Add(entity, info) if requirement.matches(info) =>
            if(_name.isDefined) {
              println(
                "[info][" + actorName + "] " +
                "Got requirement " + info.identifier.toString() +
                " from entity " +  e.getSimpleName
              )
            }
            observeSVar(entity, storage)(info)
          case change: Add[_] =>
            if(_name.isDefined) {
              println(
                "[info][" + actorName + "] " +
                "Got but does not require " + change.info.identifier +
                " from entity " +  e.getSimpleName
              )
            }
          case _ =>
        }
      }
    })
  }

  private def observeSVar(e: Entity, storage: SValSet)(info: StateParticleInfo[_]): Unit = {
    def toSVal[T](semanticType: ConvertibleTrait[T], value: Any, timestamp: simx.core.svaractor.TimedRingBuffer.Time) =
      semanticType(value.asInstanceOf[T], timestamp.timeInMillis)

    def handleNewValue[T](value: T, timestamp: simx.core.svaractor.TimedRingBuffer.Time): Unit = {

      val sValDesc = info.typeInfo match {
        case sValDescription: SValDescription[_,_,_,_] =>
          val semanticSymbol = OntologySymbol.lookup(info.identifier)
          if(semanticSymbol.isEmpty) println("[error][" + actorName + "]Could not look up " + info.identifier)
          sValDescription.as(semanticSymbol.get)
        case _ =>
          info.typeInfo
      }

      storage.getFirstSValFor(sValDesc) match {
        case Some(sVal: SVal[T@unchecked,_,_,_]) =>
          //          println("got " + sVal)
          val app = sVal.withHistoryPrependedBy(value, timestamp.timeInMillis)
          //          println("appended " + app)
          storage.replaceAllWith(app)
          //          println("result " + storage)
        case _ =>
          val sVal = toSVal(sValDesc, value, timestamp)
          storage.replaceAllWith(sVal)
      }

      onNewRequirementValue(e, info, timestamp)
    }

    onStartOfObservation(e, info)
    info.svar.observe(handleNewValue _)
    info.svar.get(handleNewValue _)
  }

  def onNewRequirementValue(e: Entity, requirementInfo: StateParticleInfo[_], timestamp: simx.core.svaractor.TimedRingBuffer.Time)
  def onStartOfObservation(e: Entity, requirementInfo: StateParticleInfo[_])

}