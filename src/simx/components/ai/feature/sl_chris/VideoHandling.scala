
package simx.components.ai.feature.sl_chris

import java.awt.image.BufferedImage
import java.io.File
import java.util.concurrent.TimeUnit

import com.xuggle.mediatool.{IMediaWriter, ToolFactory}
import com.xuggle.xuggler.ICodec
import simx.core.entity.Entity
import simx.core.entity.component.EntityCreationHandling
import simx.core.ontology.types
import simx.core.svaractor.SVarActor
import simx.core.svaractor.TimedRingBuffer.Time
import simx.core.worldinterface.eventhandling.{EventHandler, EventProvider}

/**
 * Created by martin on 5/26/2015.
 */
trait VideoHandling extends SVarActor with EventHandler with EventProvider with EntityCreationHandling {

  protected var fileBase: Option[File]

  private var videoWriter: Option[IMediaWriter] = None
  private var isRecording = false
  private var videoCursor = 0L
  private var firstFrameTimestamp: Option[Long] = None
  private var lastFrameTimestamp: Option[Long] = None
  private val videoExtension = ".mp4"
  private var videoEntity: Option[Entity] = None

  protected def startVideoRec(
    e: Entity,
    additionalImageHandler: (BufferedImage, Time) => Unit = (a,b) => {}
  ): Unit =
  {
    if(!isRecording) {
      println("[info][VideoHandling] Starting to record video to " + fileBase.get.getAbsolutePath + videoExtension)
      videoEntity = Some(e)
      isRecording = true
      videoCursor = 0L
      firstFrameTimestamp = None
      lastFrameTimestamp = None
      observeEntityWithBufferedImage(additionalImageHandler)
    } else {
      println("[info][VideoHandling] Video recording failed. Another video stream is already being recorded.")
    }
  }

  protected def stopVideoRec(): Unit = {
    if(isRecording) {
      closeVideoStream()
      videoWriter = None
      isRecording = false
      videoEntity.foreach{_.getSVars(types.Image).foreach(_._2.ignore())}
      videoEntity = None
      println("[info][VideoHandling] Stoping video recording.")
    }
  }

  private def observeEntityWithBufferedImage(additionalHandler: (BufferedImage, Time) => Unit = (a,b) => {}) ={
    def handleNewImage(newValue: BufferedImage, timestamp: Time): Unit = {
      if(isRecording) {
        appendVideoWith(newValue, timestamp.timeInMillis)
        additionalHandler(newValue, timestamp)
      }
    }
    videoEntity.foreach{_.observe(types.Image)(handleNewImage _)}
  }

  private def appendVideoWith(aImage: BufferedImage, timestamp: Long): Unit =
  {
      if(firstFrameTimestamp.isEmpty) {
        firstFrameTimestamp = Some(timestamp)
        videoWriter = Some(ToolFactory.makeWriter(fileBase.get.getAbsolutePath + videoExtension))
        videoWriter.get.addVideoStream(0, 0, ICodec.ID.CODEC_ID_MPEG4, aImage.getWidth, aImage.getHeight)
      }

      if(lastFrameTimestamp.isEmpty) {
        videoCursor = 0L
      } else {
        videoCursor += (timestamp - lastFrameTimestamp.get)
      }
      lastFrameTimestamp = Some(timestamp)

      if(aImage.getWidth == 1 && aImage.getHeight == 1)
        throw new Exception(Console.RED + "[error][VideoHandling] Received image has width and height equal to 1. " +
          "Is the camera (e.g. Kinect sensor) plugged in and properly set up?" + Console.RESET)

      //videoCursor += 1000/30
      val tConvertedImage = convertToType(aImage, BufferedImage.TYPE_3BYTE_BGR)
      videoWriter.get.encodeVideo(0,
        tConvertedImage,
        videoCursor,
        TimeUnit.MILLISECONDS)
  }

  private def convertToType(aSourceImage: BufferedImage, aTargetType: Int): BufferedImage = {
    if (aSourceImage.getType == aTargetType) {
      aSourceImage
    }
    else {
      val tImage = new BufferedImage(aSourceImage.getWidth, aSourceImage.getHeight, aTargetType)
      tImage.getGraphics.drawImage(aSourceImage, 0, 0, null)
      tImage
    }
  }

  private def closeVideoStream() {
    println("[info][VideoHandling] Recorded video with a duration of " + (lastFrameTimestamp.get-firstFrameTimestamp.get) + " ms.")
    videoWriter.collect{case writer => if(writer.isOpen) writer.close()}
  }
}
