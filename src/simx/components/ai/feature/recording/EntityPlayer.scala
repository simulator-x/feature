package simx.components.ai.feature.recording

import java.io.{FileInputStream, ObjectInputStream, File}
import java.util.UUID
import simx.components.ai.feature.recording.storage.{Persistence, StoredEntity, StoredProperty, Storage}
import simx.core.entity.Entity
import simx.core.entity.component.EntityCreationHandling
import simx.core.entity.description.SVal
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.helper.{IO, chirality}
import simx.core.ontology.{Symbols, SValDescription, EntityDescription, types}
import simx.core.svaractor.semantictrait.base.GroundedSymbolFeatures
import simx.core.svaractor.{TimedRingBuffer, SVarActor}
import simx.core.svaractor.TimedRingBuffer.At
import simx.core.svaractor.unifiedaccess.EntityUpdateHandling
import simx.core.worldinterface.WorldInterfaceActor
import simx.core.worldinterface.eventhandling.EventProvider

abstract class PlaybackMode()
case class CreateEntities()
case class RealTimePlayback() extends PlaybackMode
case class FastForwardPlayback(
  forerunInMillis: Long = 5000L,
  coolDownInMillis: Long = 3000L,
  speedUp: Long = 2L) extends PlaybackMode

/**
 * Created by
 * martin
 * in May 2015.
 */
class EntityPlayer(
  storage: Storage,
  playbackMode: PlaybackMode = RealTimePlayback(),
  nextFileDelta: Option[Long] = Some(10000L)
) extends SVarActor with EntityUpdateHandling with EntityCreationHandling with EventProvider
{

  case class PlayBackItem(timestamp: Long, entityRef: UUID, semanticValue: SVal.SValType[_]) {
    def entity: Entity = playbackEntities(entityRef).get
  }

  /**
   * Used for the fast forward playback mode.
   */
  private val minTickDuration = 5L

  private var playbackEntities = Map[UUID, Option[Entity]]()
  private var playbackData = Seq[PlayBackItem]()

  private var entitiesCreated = false
  private var isPlayingBack = false

  private var fastForward: Boolean = false
  private var forerun: Option[Long] = None
  private var coolDown: Option[Long] = None
  private var speedUp: Option[Long] = None

  addHandler[CreateEntities]{msg => createEntities()}
  addHandler[RealTimePlayback]{msg => startPlayback()}
  addHandler[FastForwardPlayback]{msg =>
    forerun = Some(msg.forerunInMillis)
    coolDown = Some(msg.coolDownInMillis)
    if(msg.speedUp != 0L) speedUp = Some(msg.speedUp)
    fastForward = forerun.isEmpty
    startPlayback()
  }

  override protected def startUp() = {
    provideEvent(Events.playbackFinished)
    new EntityDescription('Player, types.EntityType(Symbols.record)).realize{ e =>
      e.set(types.Enabled(false))
      e.observe(types.Enabled){ newValue =>
        if(newValue) self ! playbackMode 
      }
    }
    self ! CreateEntities()
  }

  private def createEntities() {
      //TODO Identify underlying error and correct to make this (dirty) workaround unnecessary
      chiralityBugWorkaround()

      createEntitiesFrom(storage)
  }

  private def createEntitiesFrom(data: Storage) = {
    if(!entitiesCreated) {
      data.entities.foreach(createEntity)

      println("[info][EntityPlayer] Sorting data by timestamps ...")
      playbackData = playbackData.sortWith(_.timestamp < _.timestamp)
      println("[info][EntityPlayer] Finished sorting " + playbackData.size + " data items by timestamps.")

      val recordingDuration = playbackData.last.timestamp - playbackData.head.timestamp
      println("[info][EntityPlayer] Recording duration is " + recordingDuration + " ms.")

      if (storage.metaData.nonEmpty) {
        val earliestPlaybackStart =
          storage.metaData.flatMap(_.recordingStart).sortWith(_ < _).head
        val recToVidOffset = earliestPlaybackStart - playbackData.head.timestamp
        println("[info][EntityPlayer] Recording to video start offset is " + recToVidOffset + " ms.")
      }
      entitiesCreated = true
    } else {
      println("[warn][EntityPlayer] Entities have already been created. Skipping additional entity creation.")
    }
  }

  private def createEntity(storedEntity: StoredEntity): Unit = {
    val entityRef = UUID.randomUUID()
    playbackEntities = playbackEntities.updated(entityRef, None)

    storedEntity.properties.foreach{ storedProperty =>
      storedProperty.toSValSeq.foreach{ tuple =>
        playbackData +:= PlayBackItem(tuple._2, entityRef, tuple._1)
      }
    }

    new Entity(new EntityDescription(name = storedEntity.name).desc).addRemoveObserver(WorldInterfaceActor.self){ e =>
      registerEntity(e)
      playbackEntities = playbackEntities.updated(entityRef, Some(e))
      storedEntity.properties.foreach{ p =>
        val firstItem = p.toSValSeq.map{i => PlayBackItem(i._2, null, i._1)}.sortWith(_.timestamp < _.timestamp).head
        e.set(firstItem.semanticValue, At(firstItem.timestamp), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})
      }
    }
  }

  def getOuter(o: Any) = Option[Any] {
    val outerField = o.getClass.getDeclaredFields.toList.find(_.getName == "$outer")
    outerField.map{f => f.setAccessible(true); f.get(o)}
  }

  def flatten(o: Option[Any]): Option[Any] = o match {
    case Some(thing @ Some(_)) => flatten(thing)
    case flat => flat
  }

  private def startPlayback(): Unit = {
    if(!isPlayingBack) {
      println("[info][EntityPlayer] Starting playback")
      forerun.foreach{f =>
        println("[info][EntityPlayer] Beginning " + f + " ms forerun in realtime.")
        addJobIn(f) {
          println("[info][EntityPlayer] Beginning fast forward playback.")
          fastForward = true
        }
      }
      if (playbackData.nonEmpty) {
        isPlayingBack = true
        setNextValue(playbackData.toArray, 0)
      }
    } else {
      println("[warn][EntityPlayer] Can not start playback. Already playing back.")
    }
  }

  private case class SetNextValueMsg(data: Array[PlayBackItem], cursor: Int)
  addHandler[SetNextValueMsg]{msg => setNextValue(msg.data, msg.cursor)}

  private var subStepSum: Option[Long] = None

  private def setNextValue(data: Array[PlayBackItem], cursor: Int): Unit = {
    val item = data(cursor)
    val e = playbackEntities(item.entityRef).get
    e.set(item.semanticValue, At(item.timestamp), TimedRingBuffer.UnbufferedTimeStoring)(_ => {})

    if(cursor+1 < data.length) {
      var deltaUntilNextSet = data(cursor+1).timestamp - item.timestamp
      if(nextFileDelta.isDefined && deltaUntilNextSet > nextFileDelta.get) {
        println(Console.GREEN + "[info][EntityPlayer] Detected delta between value changes greater than 10sec. " +
          "Assuming next playback file. Reducing delta to 500ms (real time)." + Console.RESET)
        deltaUntilNextSet = 500L
      }
      if(fastForward) {
        if(speedUp.isDefined) {
          deltaUntilNextSet = deltaUntilNextSet / speedUp.get

          if(deltaUntilNextSet + subStepSum.getOrElse(0L) < minTickDuration) {
            subStepSum = Some(deltaUntilNextSet + subStepSum.getOrElse(0L))
            deltaUntilNextSet = 0L
          } else {
            deltaUntilNextSet = deltaUntilNextSet + subStepSum.getOrElse(0L)
            subStepSum = None
          }

        }
        else deltaUntilNextSet = 0L
      }
      if(deltaUntilNextSet == 0L) {
        self ! SetNextValueMsg(data, cursor + 1)
        //setNextValue(data, cursor + 1, fastForward)
      }
      else {
        //println("Delaying next set by " + deltaUntilNextSet)
        //val now = System.currentTimeMillis()
        addJobIn(deltaUntilNextSet) {
          //println( deltaUntilNextSet + " vs. " + ( System.currentTimeMillis()-now))
          setNextValue(data, cursor+1)
        }
      }
    } else {
      println("[info][EntityPlayer] Finished playback.")
      if(coolDown.isDefined) {
        println("[info][EntityPlayer] Emitting 'playbackFinished' Event in " + coolDown.get + "ms.")
        addJobIn(coolDown.get) {emitPlaybackFinished()}
      } else emitPlaybackFinished()
    }
  }

  private def emitPlaybackFinished(): Unit = {
    println("[info][EntityPlayer] Emitting 'playbackFinished' now.")
    Events.playbackFinished.emit()
  }

  override protected def removeFromLocalRep(e: Entity): Unit = {}

  private def chiralityBugWorkaround(): Unit = {
    storage.entities.foreach{e =>
      e.properties.foreach{ p =>
        def correct[T](prop: StoredProperty[T]): Unit = {
          prop.info = types.Chirality.asInstanceOf[ConvertibleTrait[T]]
        }
        if(p.values.headOption.exists{x => x._1.isInstanceOf[chirality.Chirality]}) correct(p)
      }
      e.properties.foreach{ p =>
        def correct[T](prop: StoredProperty[T]): Unit = {
          prop.info = types.Normal.asInstanceOf[ConvertibleTrait[T]]
        }
        def isNormal(groundedSymbol: Any) = {
          val outer = flatten(getOuter(groundedSymbol)).map{_.asInstanceOf[GroundedSymbolFeatures]}
          outer.exists(_.toString == "normal")
        }
        if(isNormal(p.info.asInstanceOf[SValDescription[_,_,_,_]].valueDescription.groundedSymbol)) correct(p)
      }
    }
  }
}
