package u05lab.code

import org.junit.jupiter.api.Assertions.assertEquals
import org.junit.jupiter.api.Test

class ListTest {

  private val l = List(10, 20, 30, 40, 50)
  private val l2 = List(20, 30, 10, 5, 50)

  @Test
  def testZipRight(): Unit = {
    assertEquals(List.nil, List.nil.zipRight)
    assertEquals(List((10, 0), (20, 1), (30, 2), (40, 3), (50, 4)), l.zipRight)
  }

  @Test
  def testPartition(): Unit = {
    assertEquals((List(10, 20), List(30, 40, 50)), l.partition(_ < 25))
    assertEquals((List.nil, l), l.partition(_<5))
    assertEquals((List(10, 5), List(20, 30, 50)), l2.partition(_ <= 10))
  }

  @Test
  def testSpan(): Unit = {
    assertEquals((List(10, 20), List(30, 40, 50)), l.span(_ > 25))
    assertEquals((List(20, 30), List(10, 5, 50)), l2.span(_ <= 10))
  }

  @Test
  def testReduce(): Unit = {
    assertEquals(150, l.reduce(_ + _))
    try {
      List[Int]().reduce(_ + _)
      assert(false)
    } catch {
      case _:UnsupportedOperationException =>
    }
    assertEquals(10, List(10).reduce(_ + _))
  }

  @Test
  def testTakeRight(): Unit = {
    assertEquals(List(40, 50), l.takeRight(2))
    assertEquals(List.nil, List().takeRight(1))
  }

  @Test
  def testCollect(): Unit = {
    assertEquals(List(10, 50), l.collect { case x if x == 10 | x == 50 => x})
    assertEquals(List.nil, List[Int]().collect { case x if x == 10 => x})
  }
}
