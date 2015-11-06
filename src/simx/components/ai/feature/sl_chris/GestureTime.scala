

package simx.components.ai.feature.sl_chris

import simx.core.entity.description.SyncTime

/**
 * Created by martin on 5/26/2015.
 */
class GestureTime(gesture: String) {


  private var times: List[(Long, Long)] = Nil

  def addTime(time: (String, String), offset: Long = 0L) {
    val t1 = parseToLong(time._1)
    val t2 = parseToLong(time._2)
    if (t1.isDefined && t2.isDefined) {
      val delta: (Long, Long) = (t1.get+offset, t2.get+offset)
      times = delta :: times
    }
  }

  private def parseToLong(s: String) = try {
    Some(s.filterNot(_ == '.').toLong)
  } catch {
    case e: Exception =>
      println("[Error] GestureTimes can not parse String to Long " + e)
      None
  }

  def isPresent(implicit sync: SyncTime): Boolean =
    isGestureAtThat(sync.t0)
  
  /**
   * compares if a given time matches starttime < given time < endtime
   * @param time
   * @return
   */
  def isGestureAtThat(time: Long): Boolean = {
    var result = false
    times.foreach(t => {
      val t1 = t._1
      val t2 = t._2
      //    println(t1 + " < " + time + " < " + t2)
      if (t1 < time && time < t2)
        result = true
    })
    result
  }

  /**
   * compares if is gestureatthat(time) and if given symbol is equals this.gesture
   * @param name
   * @param time
   * @return
   */
  def isGestureNameAndTimeEquals(name: Symbol, time: Long): Boolean = {
    if (Symbol(gesture) == name && isGestureAtThat(time)) true
    else false
  }

  override def toString(): String = {
    gesture + " times: " + times.toString()
  }

}

