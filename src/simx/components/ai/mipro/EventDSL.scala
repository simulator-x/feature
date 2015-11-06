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

import simx.core.svaractor.SVarActor
import simx.core.worldinterface.eventhandling.{Event, EventDescription, EventHandler}

/**
 * Created by
 * martin
 * in September 2015.
 */
trait EventDSL extends SVarActor with EventHandler {
  private var eventHandlers: List[(EventDescription, (Event) => Unit)] = Nil

  class IncompleteEventHandler private[mipro](eventDescription: EventDescription) {
    def by(handler: (Event) => Unit): Unit = {
      eventHandlers ::= (eventDescription, handler)
    }
    def by(handler: => Unit): Unit = {
      eventHandlers ::= (eventDescription, (e: Event) => handler)
    }
  }

  object Reacts {
    def to(eventDescription: EventDescription) = new IncompleteEventHandler(eventDescription)
    def to(what: event.type) = new {
      def describedBy(eventDescription: EventDescription) = new IncompleteEventHandler(eventDescription)
    }
  }

  case object event

  override protected def startUp(): Unit = {
    super.startUp()
    eventHandlers.foreach{ tuple =>
      val eventDescription = tuple._1
      val handler = tuple._2
      eventDescription.observe(handler)
    }
  }
}
