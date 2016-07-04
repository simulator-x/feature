
package simx.components.ai.feature.sl_chris

import java.io.File
import java.net.URL
import javax.imageio.ImageIO
import java.awt.image.BufferedImage

import org.bytedeco.javacpp.avcodec
import org.bytedeco.javacv.{FrameRecorder, Java2DFrameConverter}

import simx.core.entity.Entity
import simx.core.ontology.types
import simx.core.svaractor.SVarActor
import simx.core.svaractor.TimedRingBuffer.Time
import simx.core.entity.component.EntityCreationHandling
import simx.core.worldinterface.eventhandling.{EventHandler, EventProvider}

object VideoHandlingTest{
  def main(args: Array[String]) {
    SVarActor.createActor(new VideoHandling{
      // output to test-video.mp4
      override protected var fileBase: Option[File] = Some(new File("test-video"))
      // entities not supported by testing application
      override protected def removeFromLocalRep(e: Entity): Unit = {}

      override protected def startUp(): Unit = {
        // load a fancy image
        val img = ImageIO.read(new URL("http://www.hci.uni-wuerzburg.de/assets/images/hci-logo-red-210h.png"))

        // initialize recording
        initRecording()

        // add some images
        var i = 0
        while (i < 1000){
          appendVideoWith(img, i)
          // ensure variable framerate
          i += scala.util.Random.nextInt(21) + 1
        }

        // stop recording
        deinitRecording()
        // exit gracefully
        SVarActor.shutdownSystem()
      }
    })
  }
}

/**
  *
  * Created by martin on 5/26/2015.
  * Updated by dennis on 6/10/2016.
  */
trait VideoHandling extends SVarActor with EventHandler with EventProvider with EntityCreationHandling {

  protected var fileBase: Option[File]
  private var isRecording = false
  private var frameRecorder : Option[FrameRecorder] = None
  private val converter = new Java2DFrameConverter()
  private var videoCursor = -1L
  private var firstFrameTimestamp: Long = -1L
  private var lastFrameTimestamp : Long = -1L
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
      initRecording()
      observeEntityWithBufferedImage(additionalImageHandler)
    } else {
      println("[info][VideoHandling] Video recording failed. Another video stream is already being recorded.")
    }
  }

  protected def stopVideoRec(): Unit = {
    if(isRecording) {
      deinitRecording()
      videoEntity.foreach{_.getSVars(types.Image).foreach(_._2.ignore())}
      videoEntity = None
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

  private[sl_chris] def initRecording(): Unit ={
    firstFrameTimestamp = -1L
    videoCursor = 0L
    isRecording = true
  }

  private[sl_chris] def deinitRecording(): Unit ={
    closeVideoStream()
    isRecording = false
    println("[info][VideoHandling] Stoping video recording.")
  }

  private[sl_chris] def appendVideoWith(aImage: BufferedImage, timestamp: Long): Unit =
  {
    // check if in recording mode
    if (!isRecording)
      throw new Exception(Console.RED + "[error][VideoHandling] Recording was not initialized " +
        "before adding the first image." + Console.RESET)

    // check if image is valid
    if(aImage.getWidth == 1 && aImage.getHeight == 1)
      throw new Exception(Console.RED + "[error][VideoHandling] Received image has width and height equal to 1. " +
        "Is the camera (e.g. Kinect sensor) plugged in and properly set up?" + Console.RESET)

    val recorder = frameRecorder match {
      case None =>
        // create recorder matching our image size
        val newRecorder = FrameRecorder.createDefault(fileBase.get.getAbsolutePath + videoExtension, aImage.getWidth, aImage.getHeight)
        // use h264 codec
        newRecorder.setVideoCodec(avcodec.AV_CODEC_ID_H264)
        // ensure good quality
        newRecorder.setVideoQuality(1.0)
        // allow for timestamps of 1 millisecond
        newRecorder.setFrameRate(1024)
        // start the recorder
        newRecorder.start()

        frameRecorder = Some(newRecorder)
        newRecorder
      case Some(definedRecorder) =>
        // use the defined recorder
        definedRecorder
    }

    // initialize first timestamp and video cursor
    if(firstFrameTimestamp == -1L) {
      firstFrameTimestamp = timestamp
      videoCursor = 0L
    } else {
      // update cursor for later images
      videoCursor += (timestamp - lastFrameTimestamp)
    }

    // store current timestamp as last timestamp
    lastFrameTimestamp = timestamp

    // timestamps are set in microseconds but videoCursor is in milliseconds, so multiply by 1000
    recorder.setTimestamp(videoCursor * 1000)
    // ensure the image has a compatible type
    val frame = convertToType(aImage, BufferedImage.TYPE_3BYTE_BGR)
    // actually append the image
    recorder.record(converter.getFrame(frame))
  }

  private def convertToType(aSourceImage: BufferedImage, aTargetType: Int): BufferedImage =
    if (aSourceImage.getType == aTargetType)
      aSourceImage
    else {
      val tImage = new BufferedImage(aSourceImage.getWidth, aSourceImage.getHeight, aTargetType)
      tImage.getGraphics.drawImage(aSourceImage, 0, 0, null)
      tImage
    }

  private def closeVideoStream() {
    println("[info][VideoHandling] Recorded video with a duration of " + (lastFrameTimestamp-firstFrameTimestamp) + " ms.")
    frameRecorder.collect{ case recorder => recorder.stop() }
    frameRecorder = None
  }
}
