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

package simx.components.ai.feature.recording.storage.xml

import java.io.File

import scala.xml.pull.{EvElemEnd, EvElemStart, EvText, XMLEventReader}

/**
 * Created by martin 
 * on 05/08/15.
 */
trait PullParsing {

  class EvHandler() {
    def onStart(s: EvElemStart): Unit = {}
    def onText(t: EvText): Unit = {}
    def onEnd(e: EvElemEnd): Unit = {}
  }

  case class OnText(handler: EvText => Unit) extends EvHandler {
    override def onText(t: EvText) {handler(t)}
  }

  case class OnEnd(handler: EvElemEnd => Unit) extends EvHandler {
    override def onEnd(e: EvElemEnd) {handler(e)}
  }

  private var evHandlerRegisterExactly  = Map[Exactly, EvHandler]()
  private var evHandlerRegisterAllBelow = Map[AllBelow, EvHandler]()


  abstract class Path
  object Exactly {
    def apply(label: String) = new Exactly(None, label)
    def apply(parentLabel: String, label: String) = new Exactly(Some(parentLabel), label)
  }
  case class Exactly(parent: Option[String], label: String) extends Path
  object AllBelow {
    def apply(label: String) = new AllBelow(None, label)
    def apply(parentLabel: String, label: String) = new AllBelow(Some(parentLabel), label)
  }
  case class AllBelow(parent: Option[String], label: String) extends Path

  protected def addEvHandler(p: Path, h: EvHandler): Unit = p match {
    case e: Exactly   => evHandlerRegisterExactly   += (e -> h)
    case a: AllBelow  => evHandlerRegisterAllBelow  += (a -> h)
  }

  private def getEvHandlerFor(path: List[String]): EvHandler = {
    val exactly = path match {
      case Nil => None
      case head :: Nil => evHandlerRegisterExactly.get(Exactly(None, head))
      case head :: tail => evHandlerRegisterExactly.get(Exactly(Some(tail.head), head))
    }

    def getAllBelowHandler(p: List[String]): EvHandler = p match {
      case Nil =>  new EvHandler()
      case head :: Nil => evHandlerRegisterAllBelow.getOrElse(AllBelow(None, head), new EvHandler())
      case head :: tail => evHandlerRegisterAllBelow.getOrElse(AllBelow(Some(tail.head), head), getAllBelowHandler(tail))
    }

    exactly.getOrElse(getAllBelowHandler(path))
  }

  protected def pullParse(file: File) {
    val xml = new XMLEventReader(scala.io.Source.fromFile(file))
    def loop(path: List[String], handlerStack: List[EvHandler]) {
      if (xml.hasNext) {
        xml.next() match {
          case s: EvElemStart =>
            val label   = s.label
            val newPath = label :: path
            val handler = getEvHandlerFor(newPath)
            handler.onStart(s)
            loop(newPath, handler :: handlerStack)
          case t: EvText =>
            handlerStack.head.onText(t)
            loop(path, handlerStack)
          case e: EvElemEnd =>
            handlerStack.head.onEnd(e)
            loop(path.tail, handlerStack.tail)
          case _ => loop(path, handlerStack)
        }
      }
    }
    loop(Nil, Nil)
  }
}
