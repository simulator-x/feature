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

import simplex3d.math.floatx.{ConstMat4f, ConstVec3f, ConstVec2f}
import simx.core.component.Component
import simx.core.entity.typeconversion.{TypeInfo, ConvertibleTrait}
import simx.core.entity.description._
import simx.core.entity.Entity
import simx.core.ontology.referencesystems.CoordinateSystemConverter
import simx.core.ontology.{GroundedSymbol, Symbols, types}
import simx.core.worldinterface.eventhandling.{Event, EventHandler, EventProvider}
import simplex3d.math.float._
import simx.core.entity.component.{ComponentAspect, EntityCreationHandling}
import simx.core.svaractor.SVarActor
import simx.components.ai.feature.collection.{BufferedSValSet, RingBuffer}
import scala.reflect.ClassTag
import java.io.{ObjectOutputStream, FileOutputStream, File}
import simplex3d.math.floatx.functions._
import simx.core.entity.description.NamedSValSet
import scala.Some
import simx.components.ai.feature.collection.OutOfBounds
import java.text.SimpleDateFormat
import java.util.Date
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling

object FeatureComponent {

  private var count = -1

  private[feature] def getComponentName : Symbol = synchronized {
    count = count + 1
    Symbol(local.Symbols.feature.value.toSymbol.name + "#" + count)
  }

  type inputRecordingDataType = List[(Long, Event)]
}

case class FeatureComponentAspect(
  name : Symbol = FeatureComponent.getComponentName,
  vrpnToWorldOffsetInWorldCS: ConstVec3,
  vrpnToWorldRotationInWorldCS: ConstQuat4,
  vrpnToWorldScale: ConstVec3,
  record: Boolean = false,
  recordOutputFile: File = new File("inputFeatures"),
  debug: Boolean = false
) extends ComponentAspect[FeatureComponent](local.Symbols.feature, name)
{
  def getComponentFeatures: Set[ConvertibleTrait[_]] = Set(
    local.VrpnToWorldOffsetInWorldCS,
    local.VrpnToWorldRotationInWorldCS,
    local.VrpnToWorldScale,
    local.Debug,
    local.Record,
    local.RecordOutputFile
  )
  def getCreateParams: NamedSValSet = NamedSValSet(aspectType,
    local.VrpnToWorldOffsetInWorldCS(vrpnToWorldOffsetInWorldCS),
    local.VrpnToWorldRotationInWorldCS(vrpnToWorldRotationInWorldCS),
    local.VrpnToWorldScale(vrpnToWorldScale),
    local.Debug(debug),
    local.Record(record),
    local.RecordOutputFile(recordOutputFile)
  )
}

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/2/13
 * Time: 1:47 PM
 */
class FeatureComponent(override val componentName: Symbol)
  extends Component(componentName, local.Symbols.feature) with EventProvider with EntityCreationHandling with EntityUpdateHandling {
  thisFeatureComponent =>

  //override protected implicit val actorContext = this

  private var debug = false
  private var recording = false
  private var recordOutputFile = new File("inputFeatures")
  private var recordedInputFeatures: FeatureComponent.inputRecordingDataType = List[(Long, Event)]()

  protected def finalizeConfiguration(e: Entity){
    e.observe(local.VrpnToWorldOffsetInWorldCS).head{ value => vrpnToWorldOffsetInWorldCS = Some(value) }
    e.get(local.VrpnToWorldOffsetInWorldCS).head{ value => vrpnToWorldOffsetInWorldCS = Some(value) }

    e.observe(local.VrpnToWorldRotationInWorldCS).head(value => vrpnToWorldRotationInWorldCS = Some(ConstMat4(rotationMat(value))) )
    e.get(local.VrpnToWorldRotationInWorldCS).head(value => vrpnToWorldRotationInWorldCS = Some(ConstMat4(rotationMat(value))) )

    e.observe(local.VrpnToWorldScale).head( value => vrpnToWorldScale = Some(value) )
    e.get(local.VrpnToWorldScale).head( value => vrpnToWorldScale = Some(value) )

    e.observe(local.Debug).head( value =>  debug = value)
    e.get(local.Debug).head( value =>  debug = value)

    e.observe(local.Record).head{ value => recording = value; if(!value) storeRecording()}
    e.get(local.Record).head{ value => recording = value}

    e.observe(local.RecordOutputFile).head( value => recordOutputFile = value )
    e.get(local.RecordOutputFile).head( value => recordOutputFile = value )
  }

  def storeRecording() {
    val file = dateTimeFileFrom(recordOutputFile)
    println("[info][FeatureComponent] Writing recorded data to " + file.getAbsolutePath)
    val oos = new ObjectOutputStream(new FileOutputStream(file))
    oos.writeObject(recordedInputFeatures)
    oos.close()
    recordedInputFeatures = Nil
  }

  def split(f: File) = f.getName.split('.').toList match {
    case n :: e :: Nil => (n, Some(e))
    case n :: Nil => (n, None)
    case _ => throw new Exception("[error][FeatureComponent] while parsing file " + f.getAbsolutePath)
  }

  def dateTimeFileFrom(f: File) = {
    val (name, extension) = split(f)
    new File(f.getParent, name + "-" + getDateTimeString + extension.getOrElse(""))
  }

  def getDateTimeString =
    new SimpleDateFormat("yyyy-MM-dd-HH-mm-ss").format(new Date())

  protected def requestInitialConfigValues(toProvide: Set[ConvertibleTrait[_]], aspect: EntityAspect, e: Entity) =
    aspect.getCreateParams.combineWithValues(toProvide)._1

  private var vrpnToWorldRotationInWorldCS: Option[ConstMat4] = None
  private var vrpnToWorldScale: Option[ConstVec3] = None
  private var vrpnToWorldOffsetInWorldCS: Option[ConstVec3] = None

  /**
   * (re)configure the component
   * @param params the configuration params
   */
  protected def configure(params: SValSet) {
    vrpnToWorldRotationInWorldCS = params.getFirstValueFor(types.Transformation.withAnnotations(Symbols.vrpn,Symbols.orientation))
    vrpnToWorldScale = params.getFirstValueFor(types.Vector3.withAnnotations(Symbols.vrpn, Symbols.scale))
    vrpnToWorldOffsetInWorldCS = params.getFirstValueFor(types.Origin.withAnnotations(Symbols.vrpn, Symbols.offset))
  }

  /**
   * called when the actor is started
   */
  override protected def startUp() {}

  /**
   * used to integrate the entity into the simulation
   * @param e the entity to be integrated
   * @param aspect the aspect which the component has to process
   */
  protected def entityConfigComplete(e: Entity, aspect: EntityAspect) {
    //Ignore component entity for now
    if(aspect.aspectType == Symbols.component)
      return

    aspect.aspectType match {
      case local.Symbols.inputFeature =>
        configureInputFeature(e, aspect.getCreateParams.firstSValFor(local.InputFeature).value)
      case local.Symbols.feature =>
        configureFeature(e, aspect.getCreateParams.firstSValFor(local.Feature).value)
      case local.Symbols.fakeSource =>
      case tpe =>
        println("[warn][FeatureComponent] Received unknown aspect type '" + aspect + "'. This aspect is ignored.")
    }
  }

  private def configureFeature(featureEntity: Entity, f: FeatureBase) {

    class FeatureActor extends SVarActor with EventProvider with EventHandler with EntityUpdateHandling{
      //Todo
      protected def removeFromLocalRep(e : Entity){}

      case class UpdateAwareBuffer(buffer: RingBuffer[_], var updated: Boolean = false)

      private var buffers = Map[GroundedSymbol, UpdateAwareBuffer]()

      private val providedFeatureEventDesc = FeatureEventDescription(f.description)

      private val bufferedSet = new BufferedSValSet((tpe: TypeInfo[_, _]) =>
        "[error][FeatureComponent]->[BufferedSValSet] Requested type '" + tpe +
        "' not in set. You may want to require it in your FeatureDescription.")


      override protected def startUp() {
        featureEntity.observe(local.Record).head( value => bufferedSet._rec = value )
        featureEntity.get(local.Record).head( value => bufferedSet._rec = value )
        provideEvent(providedFeatureEventDesc)
        f.requirements.foreach(req => {
          requestEvent(FeatureEventDescription(req))
          //TODO implement 'better'
          def createAndAddBuffer[T]() {
            val buffer = new RingBuffer[T](60)(req.classTag.asInstanceOf[ClassTag[T]])
            buffers += FeatureEventDescription(req).name -> UpdateAwareBuffer(buffer)
            bufferedSet.add(buffer)(req.asInstanceOf[TypeInfo[T, T]])
          }
          createAndAddBuffer[req.classTag.type]()
        })
      }

      override protected def handleEvent(e: Event) {

        val req = f.requirements.map(FeatureEventDescription.apply).find(_.name.value == e.name.value).get
        val uaBuffer = buffers(e.name)
        def helper[X](c : ConvertibleTrait[X]){
          uaBuffer.buffer.unsafePut(e.get(c).get, e.get(types.Time).get)(c.classTag)
        }
        helper(req.feature)
        uaBuffer.updated = true
        checkProduce()
      }

      private def checkProduce() {
        if(buffers.values.forall(_.updated)) {
          var t0 = Long.MaxValue
          buffers.values.foreach(b => if(b.buffer(0)._2 < t0) t0 = b.buffer(0)._2)
          buffers.values.foreach(_.buffer.t0 = t0)
          try{
            val result = f.production(bufferedSet)
            //TODO Implement 'better'
            def fillResultSVal[T](c: ConvertibleTrait[T], v: Any) = c(v.asInstanceOf[T])
            def toSVal[T](c: ConvertibleTrait[T], v: Any ) = c(v.asInstanceOf[T])
            providedFeatureEventDesc.emit(
              fillResultSVal(f.description, result), types.Time(System.currentTimeMillis()))
            featureEntity.set(toSVal(f.description, result))
          }
          catch{
            case e: OutOfBounds => if(thisFeatureComponent.debug)
              println("[warn][FeatureComponent] Buffer access out of bounds. Skipping the production of '" +
                FeatureEventDescription.stringRepFor(f.description) + "'.")
          }
          buffers.values.foreach(_.updated = false)
        }
      }
    }

    createActor(new FeatureActor(), None)((actor) =>{})(e => {})
  }

  private val inValid = Float.NaN :: Float.NegativeInfinity :: Float.PositiveInfinity :: Nil

  private def isValid(v: ConstVec3): Boolean = {
    for(i <- 0 until 3) if(inValid.contains(v(i))) return false
    true
  }

  private def configureInputFeature(inputFeatureEntity: Entity, f: InputFeatureBase) {
    if(f.description.semantics != types.Transformation.semantics) {
      val featureDesc = f.description.addAnnotations(f.annotations:_*)
      val featureEventDesc = FeatureEventDescription(featureDesc)
      provideEvent(featureEventDesc)

      def handleEntity(inputEntity: Entity) {
        inputEntity.observe(f.description).head{ newValue =>
          val now = System.currentTimeMillis()
          def fillResultSVal[T](c: ConvertibleTrait[T], v: Any) = c(v.asInstanceOf[T])
          def toSVal[T](c: ConvertibleTrait[T], v: Any ) = c(v.asInstanceOf[T])
          val event = featureEventDesc.createEvent(fillResultSVal(featureDesc, newValue), types.Time(now))
          emitEvent(event)
          if(recording)
            recordedInputFeatures = (now, event) :: recordedInputFeatures
          inputEntity.set(toSVal(featureDesc, newValue))
        }
      }

      f.source match {
        case Left(entityDesc) => entityDesc.realize(handleEntity)
        case Right(entity) => handleEntity(entity)
      }
    }
    else {
      val posDesc = types.Position.withAnnotations(f.annotations:_*)
      val oriDesc = types.Orientation.withAnnotations(f.annotations:_*)
      val posEventDesc = FeatureEventDescription(posDesc)
      val oriEventDesc = FeatureEventDescription(oriDesc)
      provideEvent(posEventDesc)
      provideEvent(oriEventDesc)

      def handleEntity(inputEntity: Entity) {
        inputEntity.observe(types.Transformation).head{ newValue =>

          /*val posM: ConstMat4 = ConstMat4(
            inverse(
              vrpnToWorldRotationInWorldCS.getOrElse(ConstMat4(Mat4x3.Identity)) *
              ConstMat4(Mat4x3.translate(vrpnToWorldOffsetInWorldCS.getOrElse(Vec3.Zero) / vrpnToWorldScale.getOrElse(ConstVec3(Vec3.One))))
            ) * newValue
          )*/

          val now = System.currentTimeMillis()
          val pos = ConstVec3f(CoordinateSystemConverter.calcPos(newValue))//ConstVec3(posM(3).xyz) * vrpnToWorldScale.getOrElse(ConstVec3(Vec3.One))

          if(isValid(pos)) {
            inputFeatureEntity.set(posDesc(pos))
            val posEvent = posEventDesc.createEvent(posDesc(pos), types.Time(now))
            emitEvent(posEvent)
            if(recording)
              recordedInputFeatures = (now, posEvent) :: recordedInputFeatures
          }

          //val oriM =    inverse(vrpnToWorldRotationInWorldCS.getOrElse(ConstMat4(Mat4x3.Identity))) *
          //  newValue * vrpnToWorldRotationInWorldCS.getOrElse(ConstMat4(Mat4x3.Identity))

          val ori = ConstQuat4(quaternion(ConstMat3(CoordinateSystemConverter.calcRot(newValue))))
          inputFeatureEntity.set(oriDesc(ori))
          val oriEvent = oriEventDesc.createEvent(oriDesc(ori), types.Time(now))
          emitEvent(oriEvent)
          if(recording)
            recordedInputFeatures = (now, oriEvent) :: recordedInputFeatures
        }
      }


      f.source match {
        case Left(entityDesc) => entityDesc.realize(handleEntity)
        case Right(entity) => handleEntity(entity)
      }

      f.relativeTo.collect{case rt =>
        val rtPosDesc = types.Position.withAnnotations(rt:_*)
        val rtOriDesc = types.Orientation.withAnnotations(rt:_*)
        val rPosDesc = types.Position.withAnnotations(f.annotations ++ Seq(Symbols.relative):_*)
        val rOriDesc = types.Orientation.withAnnotations(f.annotations ++ Seq(Symbols.relative):_*)

        new RelativePositionFeature(posDesc, rtPosDesc, rPosDesc, rtOriDesc).realize()

        new RelativeOrientationFeature(oriDesc, rOriDesc, rtOriDesc).realize()
      }
    }
  }

  private def removeScale(t: ConstMat3): ConstMat3 = {
    val scale = Vec3(1f/length(t(0).xyz), 1f/length(t(1).xyz), 1f/length(t(2).xyz))
    t * ConstMat3(Mat4x3.scale(scale))
  }


//  private def toAngles(newValue: ConstMat4) = {
//
//    var xAngle = 0f
//    var yAngle = 0f
//    var zAngle = 0f
//
//    val m: ConstMat4 = newValue
//
//    if ((1f - scala.math.abs(m.m20)) >= 0.0001f) {
//      yAngle = -1.0f * scala.math.asin(m.m20).toFloat
//      xAngle = scala.math.atan2(m.m21 / scala.math.cos(yAngle), m.m22 / scala.math.cos(yAngle)).toFloat
//      zAngle = scala.math.atan2(m.m10 / scala.math.cos(yAngle), m.m00 / scala.math.cos(yAngle)).toFloat
//    }
//    else {
//      zAngle = 0
//      if (m.m20 < 0) {
//        yAngle = math.Pi.toFloat / 2f
//        xAngle = scala.math.atan2(m.m01, m.m02).toFloat
//      }
//      else {
//        yAngle = math.Pi.toFloat / -2f
//        xAngle = scala.math.atan2(-1f * m.m01, -1f * m.m02).toFloat
//      }
//    }
//
////    val x = if (functions.degrees(xAngle) >= 0) functions.degrees(xAngle) else functions.degrees(xAngle) + 360f
////    val y = if (functions.degrees(yAngle) >= 0) functions.degrees(yAngle) else functions.degrees(yAngle) + 360f
////    val z = if (functions.degrees(zAngle) >= 0) functions.degrees(zAngle) else functions.degrees(zAngle) + 360f
//    val x = functions.degrees(xAngle)
//    val y = functions.degrees(yAngle)
//    val z = functions.degrees(zAngle)
//
//    ConstVec3i(x.toInt,y.toInt,z.toInt)
//  }

  /**
   * Called for each simulation step the component should execute. The frequency with which this method is called
   * depends on the used [[simx.core.component.ExecutionStrategy]].
   */
  protected def performSimulationStep() {}

  /**
   * method to be implemented by each component. Will be called when an entity has to be removed from the
   * internal representation.
   * @param e the Entity to be removed
   */
  protected def removeFromLocalRep(e: Entity) {}

  /**
   * provideInitialValues has to be called within this method with the full set of initial values to be provided
   * @note the component should create its local representation within this method
   * @param toProvide the convertibletraits for which values shall be provided
   * @param aspect the aspect providing the context for this method call
   * @param e the entity to be filled
   * @param given a set of create parameters that were already provided
   *
   */
  protected def requestInitialValues(
    toProvide: Set[ConvertibleTrait[_]],
    aspect: EntityAspect,
    e: Entity,
    given: SValSet)
  {
    //val providings = aspect.getCreateParams.combineWithValues(toProvide)._1

    // FIXME: Provide initial values
//    aspect.getCreateParams.getFirstSValFor(local.Feature).collect{case feature =>
//      providings.addDefaultValueForIfNew(feature.value.description) }
//
//    aspect.getCreateParams.getFirstSValFor(local.InputFeature).collect{case inputFeature =>
//      if(inputFeature.value.description.semantics == types.Transformation.semantics) {
//        providings.addDefaultValueForIfNew(local.Orientation)
//        providings.addDefaultValueForIfNew(types.Position)
//      } else {
//        providings.addDefaultValueForIfNew(inputFeature.value.description.addAnnotations(inputFeature.value.annotations:_*))
//      }
//    }
    //Todo: Implement properly
    val providings = toProvide.map(tp => {
      if(tp.classTag == scala.reflect.classTag[simplex3d.math.floatx.ConstVec3f])
        tp.asInstanceOf[ConvertibleTrait[simplex3d.math.floatx.ConstVec3f]].apply(Vec3.Zero)
      else if (tp.classTag == scala.reflect.classTag[simplex3d.math.floatx.ConstQuat4f])
        tp.asInstanceOf[ConvertibleTrait[simplex3d.math.floatx.ConstQuat4f]].apply(Quat4.Identity)
      else if (tp.classTag == scala.reflect.classTag[simplex3d.math.floatx.ConstMat4f])
        tp.asInstanceOf[ConvertibleTrait[simplex3d.math.floatx.ConstMat4f]].apply(Mat4.Identity)
      else if (tp.classTag == scala.reflect.classTag[scala.Boolean])
        tp.asInstanceOf[ConvertibleTrait[scala.Boolean]].apply(false)
      else if (tp.classTag == scala.reflect.classTag[scala.Float])
        tp.asInstanceOf[ConvertibleTrait[scala.Float]].apply(0f)
      else if (tp.classTag == scala.reflect.classTag[scala.Int])
        tp.asInstanceOf[ConvertibleTrait[scala.Int]].apply(0)
      else if (tp.classTag == scala.reflect.classTag[simplex3d.math.floatx.ConstVec2f])
        tp.asInstanceOf[ConvertibleTrait[simplex3d.math.floatx.ConstVec2f]].apply(Vec2.Zero)
      else if (tp.classTag == scala.reflect.classTag[(java.lang.Boolean, simplex3d.math.floatx.ConstVec2f, simplex3d.math.floatx.ConstVec2f)]) {
        val tmp: (Boolean, ConstVec2, ConstVec2) = (false, Vec2.Zero, Vec2.Zero)
        tp.asInstanceOf[ConvertibleTrait[(Boolean, ConstVec2f, ConstVec2f)]].apply(tmp)
      }
      else
        throw new Exception("[FeatureComponent] No default value specified for " + tp)
    }.asInstanceOf[SVal.SValType[_]])

    provideInitialValues(e, SValSet(providings.toSeq:_*))
  }
}

class RelativePositionFeature( posDesc : ConvertibleTrait[ConstVec3],
                               rtPosDesc : ConvertibleTrait[ConstVec3],
                               rPosDesc : ConvertibleTrait[ConstVec3],
                               rtOriDesc : ConvertibleTrait[ConstQuat4] ) extends Feature(rPosDesc) {
  def requirements = posDesc :: rtPosDesc :: rtOriDesc ::  Nil

  def production(i: BufferedSValSet) = {
    var pos = i(posDesc) at 0
    val rtPos = i(rtPosDesc) at 0
    val rtOri = i(rtOriDesc) at 0
    pos = pos - rtPos
    val rtMat = Mat4(rotationMat(rtOri))
    val res = rtMat * ConstVec4(pos, 1)
    ConstVec3(res.xyz)
  }

}

class RelativeOrientationFeature( oriDesc : ConvertibleTrait[ConstQuat4],
                                  rOriDesc : ConvertibleTrait[ConstQuat4],
                                  rtOriDesc : ConvertibleTrait[ConstQuat4])  extends Feature(rOriDesc) {
  def requirements = oriDesc :: rtOriDesc :: Nil

  def production(i: BufferedSValSet) = {
    //TODO Check if correct
    val ori = i(oriDesc) at 0
    val rtOri = i(rtOriDesc) at 0
    rtOri * ori
  }
}
