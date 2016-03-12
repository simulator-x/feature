package simx.components.ai.feature.sl_chris

import java.io.{File, FileInputStream, ObjectInputStream}

import simx.components.ai.feature.recording
import simx.components.ai.feature.recording.storage.{StorageMetaData, Persistence, Storage}
import simx.core.helper.IO
import simx.core.worldinterface.eventhandling.EventHandler

import scala.io.Source
import scala.swing.FileChooser

/**
 * Created by
 * martin, maximilian, and chris
 * in June 2015.
 */

object AnnotationSource {
  def apply(annotationFile: File, recordingStart: Option[Long]) =
    new AnnotationSource(Some(annotationFile), recordingStart)

  def defaultFileMapper(playbackDataFile: File) =
    IO.changeExtension(playbackDataFile, "csv")

  def from(storageMetaData: StorageMetaData, fileMapper: File => File = defaultFileMapper) =
    new AnnotationSource(storageMetaData.storageFile.map(fileMapper), storageMetaData.recordingStart)
}

class AnnotationSource(val annotationFile: Option[File], val recordingStart: Option[Long])

/**
 * @note  Annotations a typically relative to the runtime of the corresponding video (0 ms).
 *        In order to correctly refer annotations to recorded data (e.g. user movements),
 *        the start time of the corresponding video has to be applied as an offset.
 *        If the standard Simulator X entity recording mechanism is used, this start time can be obtained from
 *        the loaded entity recording file via Storage.recordingStart.
 */
class AnnotationReader(sources: List[AnnotationSource]) {

  /**
   * @param annotationFile  A comma separated values (csv) file, that contains annotations.
   * @param recordingStart  Start time (i.e. timestamp) of the video that the annotations correspond to.
   */
  def this(annotationFile: File, recordingStart: Long) =
    this(AnnotationSource(annotationFile, Some(recordingStart)) :: Nil)

  /**
   * Convenience
   */
  def this(annotationFile: File, recordingStart: List[StorageMetaData]) =
    this(AnnotationSource(annotationFile, Some(recordingStart.head.recordingStart.get)) :: Nil)

  protected var gestures = Map[Symbol, GestureTime]()

  runTimeInputReader()

  def gesture(gestureAnnotation: String) = gestures(Symbol(gestureAnnotation))
  def apply(gestureAnnotation: String) = gesture(gestureAnnotation)

  private def runTimeInputReader() {
    sources.foreach{ source =>
      if(source.annotationFile.isDefined)
        readTimeFile(source.annotationFile.get, source.recordingStart.getOrElse(0L))
    }
  }

  private def chooseFile(note:String = ""): Option[File] = {
    val chooser = new FileChooser(new File("."))
    val addon = note+" "
    chooser.title = addon+"Please choose an Annotation file!"
    val result = chooser.showOpenDialog(null)
    if (result == FileChooser.Result.Approve) {
      Some(chooser.selectedFile)
    } else None
  }

  /**
   * line pattern
   * | start; end; gesturename
   */
  private def handleLineCSV(line: String, recordingStart: Long) = {
    if (!line.startsWith("\"")) {
      val lineArray = line.split(";")
      if (lineArray.length == 3) {
        val startTime = lineArray(0)
        val endTime = lineArray(1)
        val gesture = lineArray(2).replace("\"", "")
        handleGestureTime(gesture, startTime, endTime, recordingStart)
      }
    }
  }

  /**
   * line pattern
   * |  start   end   duration    gesturename|
   */
  private def handleLineTXT(l: String, recordingStart: Long) = {
    // if there is a first tab remove that
    var line = l
    if (line.startsWith("\t")) line = line.replaceFirst("\t", "")
    // then split line with tabs
    val lineArray = line.split("\t")
    if (lineArray.length == 4) {
      val startTime = lineArray(0).replace(":", "") // pattern is hh:mm:ss.ms -> hhmmss.ms
      val endTime = lineArray(1).replace(":", "")
      val gesture = lineArray(3)
      val duration = lineArray(2)
      handleGestureTime(gesture, startTime, endTime, recordingStart)
    }
  }

  private def extHandlingMethods: Map[String, (String, Long) => Unit] = Map(
    "csv" -> handleLineCSV, "txt" -> handleLineTXT)

  private def handleGestureTime(gesture: String, startTime: String, endTime: String, recordingStart: Long) {
    gestures.get(Symbol(gesture)) match {
      case Some(gt) =>
        gt.addTime((startTime, endTime), recordingStart)

      case _ =>
        val gestureTime = new GestureTime(gesture)
        gestureTime.addTime((startTime, endTime), recordingStart)
        gestures += Symbol(gesture) -> gestureTime

    }
  }

  /**
   * no extension needed
   */
  private def readTimeFile(annotationFile: File, recordingStart: Long) {
    println("[info][TimeInputReader] reading " + annotationFile)
    val (name, ext) = IO.split(annotationFile)
    def notSupported =
      throw new Exception("[error][TimeInputReader] unsupported file extension! " +
        "Only the following extensions are supported: " + extHandlingMethods.keys.mkString(", "))
    val method =
      ext.flatMap(extHandlingMethods.get).getOrElse(notSupported)
    Source.fromFile(annotationFile).getLines().foreach(method(_, recordingStart))
    println("[info][TimeInputReader] finished reading gestures " +
      gestures.keys.mkString("[", ", ", "]") + " from " + annotationFile)
  }

  override def toString = this.gestures.mkString("\n")

}