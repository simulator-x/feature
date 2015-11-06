package simx.components.ai.feature.collection

import scala.reflect.ClassTag
import simplex3d.math.float._
import simplex3d.math.float.functions._
import simplex3d.math.floatx.ConstVec2f

/**
 * Created by IntelliJ IDEA.
 * User: martin
 * Date: 9/4/13
 * Time: 5:04 PM
 */

case class OutOfBounds()
  extends Exception("[error][RingBuffer] Access of uninitialized buffer data or " +
    "a value that exceeds the buffer's time history.")

class RingBuffer[T](val size: Int)(implicit classTag: ClassTag[T]) {

  private val data = new Array[T](size)
  private val timestamps = new Array[Long](size)
  private var cursor = -1

  //TODO implement 'better'
  def apply(i: Int) = {
    if(i >= size) throw OutOfBounds()
    var idx = cursor-i
    while(idx<0)idx += size
    idx%=size
    if(data(idx) == null) throw OutOfBounds()
    (data(idx), timestamps(idx))
  }

  def put(value: T, t: Long) {
    cursor = (cursor + 1) % size
    data(cursor) = value
    timestamps(cursor) = t
  }

  def unsafePut[V](value: V, t: Long)(localClassTag : ClassTag[V]) {
    if (localClassTag.equals(classTag))
      put(value.asInstanceOf[T], t)
    else
      throw new Exception("[error][RingBuffer] Incompatible value type.")
  }

  private[feature] var t0 = System.currentTimeMillis()

  /**
   * @param elapsedTimeInMillis 0 = oldest timestamp of the latest update cycle //TODO get more precise
   */
  def at(elapsedTimeInMillis: Long): T = {
    val targetTime = t0 - elapsedTimeInMillis
    var older = 0
    while((apply(older)._2 > targetTime) && (older < size)) older += 1
    if(older == 0) apply(0)._1
    else {
      val newer = older - 1
      interpolate(older, newer, targetTime)
    }
  }

  private def interpolate(o: Int, n: Int, targetTime: Long): T = {
    val older = apply(o)
    val newer = apply(n)
    val ratio: Float = (targetTime - older._2).toFloat / (newer._2 - older._2).toFloat
    older._1 match{
      case olderValue: ConstVec3 =>
        val newerValue = newer._1.asInstanceOf[ConstVec3]
        ConstVec3(olderValue + ((newerValue - olderValue) * ratio)).asInstanceOf[T]
      case olderValue: ConstVec2 =>
        val newerValue = newer._1.asInstanceOf[ConstVec2]
        ConstVec2(olderValue + ((newerValue - olderValue) * ratio)).asInstanceOf[T]
      case olderValue: Float =>
        val newerValue = newer._1.asInstanceOf[Float]
        (olderValue + ((newerValue - olderValue) * ratio)).asInstanceOf[T]
      case olderValue: Int =>
        val newerValue = newer._1.asInstanceOf[Int]
        (olderValue + ((newerValue - olderValue).toFloat * ratio)).toInt.asInstanceOf[T]
      case olderValue: ConstQuat4 =>
        val newerValue = newer._1.asInstanceOf[ConstQuat4]
        ConstQuat4(slerp(olderValue, newerValue, ratio)).asInstanceOf[T]
      case olderValue: java.lang.Boolean =>
        val newerValue = newer._1.asInstanceOf[java.lang.Boolean]
        (if(ratio >= 0.5f) olderValue else newerValue).asInstanceOf[T]
      case _ => throw new Exception("[error][RingBuffer] No interpolation rule for type '" + classTag + "' found.")
    }
  }

  override def toString = (for(i <- 0 until size) yield apply(i)).
    mkString("RingBuffer[" + classTag + "](", ", ", ")")

}
