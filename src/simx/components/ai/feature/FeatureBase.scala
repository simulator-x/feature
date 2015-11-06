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

package simx.components.ai.feature

import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.entity.description.{SValSet, NamedSValSet, EntityAspect}
import simx.core.ontology.types._
import simx.core.ontology.EntityDescription
import simx.core.ontology.GroundedSymbol
import simx.core.ontology.types
import simx.components.ai.feature.collection.BufferedSValSet
import simx.core.worldinterface.naming.NameIt
import simx.core.entity.Entity
import simx.core.entity.component.EntityCreationHandling

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/3/13
 * Time: 12:05 PM
 */

object ImplicitEitherConversion {
  implicit def left2Either[A,B](a:A):Either[A,B] = Left(a)
  implicit def right2Either[A,B](b:B):Either[A,B] = Right(b)
}

trait FeatureBase extends Serializable {
  val description: ConvertibleTrait[_]
  def requirements: List[ConvertibleTrait[_]]
  def production(input : BufferedSValSet) : description.dataType
}

class NullFeature() extends FeatureBase {
  val description = NullType.as(local.Symbols.feature).withType(classOf[Symbol])
  def production(requiredInput: BufferedSValSet) = 'NullFeature
  val requirements = Nil
}

/**
 *  targetDescription has to provide types.Transformation (e.g. 'simx.components.vrpn.devices.TrackingTarget')
 */
trait InputFeatureBase extends Serializable {
  val description: ConvertibleTrait[_]
  def source: Either[EntityDescription, Entity]
  def annotations: Seq[GroundedSymbol]
  def relativeTo: Option[Seq[GroundedSymbol]]
}

class InputNullFeature() extends InputFeatureBase {
  val description = NullType.as(local.Symbols.feature).withType(classOf[Symbol])
  def source = Left(new EntityDescription())
  def annotations = Seq()
  def relativeTo = None
}

trait Realizable{
  def realize(h : Entity => Any = _ => {})( implicit entityCreationContext : EntityCreationHandling )
}

abstract class Feature[T](val featureDescription: ConvertibleTrait[T])
  extends FeatureBase with Realizable with Serializable
{
  thisFeatureDescription =>

  //Just renaming
  val description = featureDescription

  def featureAspect = new EntityAspect(local.Symbols.feature, local.Symbols.feature) {
    def getCreateParams = addCVars(SValSet(local.Feature(thisFeatureDescription), local.Record(false)))
    def getProvidings = Set(description, local.Record)
    def getFeatures = Set(description, local.Record)
  }

  def aspects: List[EntityAspect] =
    List(featureAspect, NameIt("Feat[" + FeatureEventDescription.stringRepFor(featureDescription) + "]"))

  def realize(h : Entity => Any = _ => {})( implicit entityCreationContext : EntityCreationHandling ) {new EntityDescription(aspects).realize(h)}
}

abstract class InputFeature[T](val featureDescription: ConvertibleTrait[T], annotation: GroundedSymbol, additionalAnnotations: GroundedSymbol*)
  extends InputFeatureBase with Realizable with Serializable
{
  thisFeatureDescription =>

  //Just renaming
  val description = featureDescription

  def annotations = Seq(annotation) ++ additionalAnnotations

  def inputFeatureAspect = new EntityAspect(local.Symbols.feature, local.Symbols.inputFeature) {
    def getCreateParams = addCVar(local.InputFeature(thisFeatureDescription))
    def getProvidings =
      if(description.semantics == types.Transformation.semantics)
        Set(types.Position.withAnnotations(annotations:_*), types.Orientation.withAnnotations(annotations:_*))
      else
        Set(description.addAnnotations(annotations:_*))
    def getFeatures = getProvidings
  }

  def aspects: List[EntityAspect] =
    List(inputFeatureAspect, NameIt("InpF[" + annotations.map(_.value.toString).mkString(",") + "]"))

  def realize(h : Entity => Any = _ => {})( implicit entityCreationContext : EntityCreationHandling ) {new EntityDescription(aspects).realize(h)}
}

case class FakeSource[T](sourceDescription: ConvertibleTrait[T], entityName: Option[String] = None)
  extends EntityDescription (
    new EntityAspect(local.Symbols.feature, local.Symbols.fakeSource) {
    override def getCreateParams = NamedSValSet(aspectType)
    override def getFeatures = Set(sourceDescription)
    override def getProvidings = getFeatures
    },
    NameIt("FakeSource" + entityName.fold("")("[" + _ + "]"))
  )