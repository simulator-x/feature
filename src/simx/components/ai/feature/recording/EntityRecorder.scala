

package simx.components.ai.feature.recording

import java.awt.image.BufferedImage
import java.io.{File, FileOutputStream, ObjectOutputStream}
import java.text.SimpleDateFormat
import java.util.{Date, UUID}

import simx.components.ai.feature.recording.storage._
import simx.components.ai.feature.sl_chris.VideoHandling
import simx.core.entity.Entity
import simx.core.entity.description.SVal
import simx.core.entity.typeconversion.ConvertibleTrait
import simx.core.helper.IO
import simx.core.ontology.types.OntologySymbol
import simx.core.ontology.{Symbols, EntityDescription, SValDescription, types}
import simx.core.svaractor.SVarActor
import simx.core.svaractor.TimedRingBuffer.Time
import simx.core.svaractor.unifiedaccess.{EntityUpdateHandling, StateParticleInfo}

/**
 * Created by martin on 5/20/2015.
 */
class EntityRecorder(
    private var recordedEntities: Set[Entity] = Set(),
    private var defaultFolder: File = new File("files/entities/entityRecording")
  ) extends SVarActor with EntityUpdateHandling with VideoHandling {

  private var isRecording: Boolean = false
  //File name with no extension
  protected var fileBase: Option[File] = None
  //private val fileExtension = ".bin"
  private val fileExtension = ".xml"

  override protected def removeFromLocalRep(e: Entity): Unit = {}

  addHandler[Record]{msg => msg.entities.foreach(addToRecording)}
  
  private def addToRecording(e: Entity) {
    if(!isRecording) {
      if (!recordedEntities.contains(e)){
        println("[info][EntityRecorder] Added to record list: " + e.getSimpleName)
        recordedEntities += e
      }
    } else {
      println("[warn][EntityRecorder] Cannot add to entities while recording.")
    }
  }

  addHandler[StartEntityRecording]{msg => startRecording(msg.folder)}
  
  private def startRecording(folder: File) = {
    if(!isRecording){
      if(recordedEntities.nonEmpty){
        if(!folder.exists()) {
          println("[info][EntityRecorder] Creating folder " + folder.getAbsolutePath)
          folder.mkdirs()
        }
        fileBase = Some(IO.dateTimeFileFrom(new File(folder, "entityRecording")))
        println("[info][EntityRecorder] Starting to record to " + fileBase.get.getAbsoluteFile + fileExtension)
        isRecording = true
        recordStartTime = System.currentTimeMillis()
        startObserving(recordedEntities)
      } else {
        println("[warn][EntityRecorder] Nothing to record.")
      }
    } else {
      println("[warn][EntityRecorder] Already recording.")
    }

  }

  addHandler[StopEntityRecording]{msg => stopRecording()}

  private def stopRecording() = {
    if(isRecording) {
      println("[info][EntityRecorder] Stopped.")
      isRecording = false
      if(fileBase.isDefined) {
        println("[info][EntityRecorder] Writing to file: " + fileBase.get.getAbsolutePath + fileExtension)
        stopVideoRec()
        //val oos = new ObjectOutputStream(new FileOutputStream(new File(fileBase.get.getAbsolutePath + fileExtension)))
        //oos.writeObject(storage)
        //oos.close()
        PersistenceOptimized.saveToFile(storage, new File(fileBase.get.getAbsolutePath + fileExtension))
        println("[info][EntityRecorder] Finished writing to file: " + fileBase.get.getAbsolutePath + fileExtension)
        storage = new Storage()
      } else {
        println("[error][EntityRecorder] No output file defined.")
      }
    } else {
      println("[warn][EntityRecorder] Can not stop, because it is not running.")
    }
  }

  override protected def startUp() = {
    println("[info][EntityRecorder] Initializing")
    recordedEntities.foreach(addToRecording)

    new EntityDescription('Recorder, types.EntityType(Symbols.record)).realize{ e =>
      e.set(types.Enabled(false))
      e.observe(types.Enabled){ newValue =>
        if(newValue)
          self ! StartEntityRecording(defaultFolder)
        else
          self ! StopEntityRecording()
      }
    }
  }

  var storage = new Storage()
  private var recordStartTime = 0L

  def startObserving(entities: Set[Entity]): Unit = {
    storage = new Storage()
    entities.foreach(handleEntity)
  }

  private def handleEntity(e: Entity): Unit = {
    //Use special recorder for entities containing images -> video
    if(e.getSVars(types.Image).nonEmpty) {
      def onNewImage(image: BufferedImage, timestamp: Time) {
        //Store timestamp of first image that is used for the video
        if(storage.metaData.isEmpty)
          storage.metaData ::= StorageMetaData(None, Some(timestamp.timeInMillis))
      }
      startVideoRec(e, onNewImage)
    }
    //Record all state particles
    else {
      val storedEntity = StoredEntity(e.id, e.getSimpleName)
      storage.entities += storedEntity
      e.getAllStateParticles.filterNot{ info =>
        val exclude = info.typeInfo.isSubtypeOf(types.Relation) || info.typeInfo.isSubtypeOf(types.Entity)
        //if(exclude) println("[info][EntityRecorder] Excluded " + info)
        exclude
      }.foreach{
        info => handleStateParticle(storedEntity)(info)
      }
    }
  }

  private def handleStateParticle[T](storedEntity: StoredEntity)(info: StateParticleInfo[T]): Unit = {
    val desc = info.typeInfo match {
      case s: SValDescription[T,_,_,_] => s.as(OntologySymbol.lookup(info.identifier).get)
      case _ => info.typeInfo
    }

    val storedProperty = new StoredProperty[T](desc)
    storedEntity.properties ::= storedProperty

    def handleSVar(description: ConvertibleTrait[T])(value: T, timestamp: Time): Unit = {
      val t = if(timestamp.timeInMillis < recordStartTime) recordStartTime else timestamp.timeInMillis
      storedProperty.values ::= (value, t)
    }
    info.svar.ignore()
    info.svar.observe(handleSVar(info.typeInfo) _)
    info.svar.get(handleSVar(info.typeInfo) _)
  }
}

case class StartEntityRecording(folder: File = new File("files/entities/entityRecording"))
case class StopEntityRecording()
case class Record(entities: Entity*)






