package u05lab.code

import java.util.concurrent.TimeUnit
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.FiniteDuration

object PerformanceUtils {
  case class MeasurementResults[T](result: T, duration: FiniteDuration) extends Ordered[MeasurementResults[_]] {
    override def compare(that: MeasurementResults[_]): Int = duration.toNanos.compareTo(that.duration.toNanos)
  }

  def measure[T](msg: String)(expr: => T): MeasurementResults[T] = {
    val startTime = System.nanoTime()
    val res = expr
    val duration = FiniteDuration(System.nanoTime()-startTime, TimeUnit.NANOSECONDS)
    if(msg.nonEmpty) println(msg + " -- " + duration.toNanos + " nanos; " + duration.toMillis + "ms")
    MeasurementResults(res, duration)
  }

  def measure[T](expr: => T): MeasurementResults[T] = measure("")(expr)
}


object CollectionsTest extends App {
  import scala.collection.immutable._

  /* Linear sequences: List, ListBuffer */
  var lst1 = List[Int]()
  val lstBuf = ListBuffer[Int]()

  for (i <- 1 to 1000000) {
    lst1 = i :: lst1
    lstBuf += i
  }

  /* Indexed sequences: Vector, Array, ArrayBuffer */
  var arr = Array[Int]()
  var arrBuf = ArrayBuffer[Int]()

  for (i <- 1 to 100) {
    arr = arr :+ i
    arrBuf += i
  }

  /* Sets */

  /* Maps */

  /* Comparison */
  import PerformanceUtils._
  val lst = (1 to 1000000).toList
  val vec = (1 to 1000000).toVector
  assert( measure("lst last"){ lst.last } > measure("vec last"){ vec.last } )

  assert( measure("lst prepend"){ lst1 = 2 +: lst1 } > measure("lstBuf prepend"){ 10 +=: lstBuf })
  assert( measure("lst append"){ lst1 = 2 :: lst1 } < measure("lstBuf append"){ lstBuf += 10 })

  assert( measure("arr append"){ arr = arr :+ 2 } > measure("arrBuf append"){ arrBuf += 2 })
  assert( measure("arr prepend"){ arr = 2 +: arr } > measure("arrBuf prepend"){ 2 +=: arrBuf})
}
