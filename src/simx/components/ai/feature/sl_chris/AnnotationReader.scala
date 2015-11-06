package simx.components.ai.feature.sl_chris

import java.io.{File, FileInputStream, ObjectInputStream}

import simx.components.ai.feature.recording
import simx.components.ai.feature.recording.storage.{Persistence, Storage}
import simx.core.helper.IO
import simx.core.worldinterface.eventhandling.EventHandler

import scala.io.Source
import scala.swing.FileChooser

/**
 * Created by
 * martin, maximilian, and chris
 * in June 2015.
 */

/**
 *
 * @param annotationFile  A comma separated values (csv) file, that contains annotations.
 * @param recordingStart  Start time (i.e. timestamp) of the video that the annotations correspond to.
 * @note  Annotations a typically relative to the runtime of the corresponding video (0 ms).
 *        In order to correctly refer annotations to recorded data (e.g. user movements),
 *        the start time of the corresponding video has to be applied as an offset.
 *        If the standard Simulator X entity recording mechanism is used, this start time can be obtained from
 *        the loaded entity recording file via Storage.recordingStart.
 */
class AnnotationReader(annotationFile: File, recordingStart: Long) {

  protected var gestures = Map[Symbol, GestureTime]()

  runTimeInputReader(annotationFile)


  def gesture(gestureAnnotation: String) = gestures(Symbol(gestureAnnotation))
  def apply(gestureAnnotation: String) = gesture(gestureAnnotation)

  /**
   * for example read file and direct print single gestures with its times from annotation
   */
  private def runTimeInputReader(annotationFile: File) {
    readTimeFile(annotationFile)
  }

  /**
   * read annotation file with file chooser
   */
  protected def runTimeInputReader() {
    val file = chooseFile()
    file match {
      case Some(f) =>
        readTimeFile(f)
      case None => chooseFile("[ERROR] try again!")
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
  private def handleLineCSV(line: String) = {
    if (!line.startsWith("\"")) {
      val lineArray = line.split(";")
      if (lineArray.length == 3) {
        val startTime = lineArray(0)
        val endTime = lineArray(1)
        val gesture = lineArray(2).replace("\"", "")
        handleGestureTime(gesture, startTime, endTime)
      }
    }
  }

  /**
   * line pattern
   * |  start   end   duration    gesturename|
   */
  private def handleLineTXT(l: String) = {
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
      handleGestureTime(gesture, startTime, endTime)
    }
  }


  private def extHandlingMethods: Map[String, String => Unit] = Map(
    "csv" -> handleLineCSV, "txt" -> handleLineTXT)


  private def handleGestureTime(gesture: String, startTime: String, endTime: String) {
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
  private def readTimeFile(file: File) {
    println("[info][TimeInputReader] reading " + file)
    //val ext = file.split("\\.")(1)
    val (name, ext) = IO.split(file)
    val m = ext.flatMap(extHandlingMethods.get)
    if (m.isDefined) {
      Source.fromFile(file).getLines().foreach(m.get)
    } else {
      println("[error][TimeInputReader] unsupported file extension! only the following extensions are supported: " + extHandlingMethods.keys.mkString(", "))
    }
    println("[info][TimeInputReader] finished reading gestures " + gestures.keys.mkString("[", ", ", "]") + " from " + file)
  }


//  private def addHandlingMethod(extension: String, handlingFunction: String => Unit) {
//    extHandlingMethods += extension -> handlingFunction
//  }

  override def toString = this.gestures.mkString("\n")

}