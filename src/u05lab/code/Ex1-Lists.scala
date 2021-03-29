package u05lab.code

import scala.::
import scala.annotation.tailrec
import scala.language.postfixOps // silence warnings

sealed trait List[A] {

  def head: Option[A]

  def tail: Option[List[A]]

  def append(list: List[A]): List[A]

  def foreach(consumer: (A) => Unit): Unit

  def get(pos: Int): Option[A]

  def filter(predicate: (A) => Boolean): List[A]

  def map[B](fun: (A) => B): List[B]

  def toSeq: Seq[A]

  def foldLeft[B](acc: B)(f: (B,A)=>B): B

  def foldRight[B](acc: B)(f: (A,B)=>B): B

  def flatMap[B](f: A => List[B]): List[B]

  def reverse(): List[A]

  def zipRight: List[(A,Int)]

  def partition(pred: A => Boolean): (List[A],List[A])

  def span(pred: A => Boolean): (List[A],List[A])

  def reduce(op: (A,A)=>A): A

  def takeRight(n: Int): List[A]

  def collect[B](partial: PartialFunction[A, B]): List[B]

  // right-associative construction: 10 :: 20 :: 30 :: Nil()
  def ::(head: A): List[A] = Cons(head,this)
}

// defining concrete implementations based on the same template

case class Cons[A](_head: A, _tail: List[A])
  extends ListImplementation[A]

case class Nil[A]()
  extends ListImplementation[A]

// enabling pattern matching on ::

object :: {
  def unapply[A](l: List[A]): Option[(A,List[A])] = l match {
    case Cons(h,t) => Some((h,t))
    case _ => None
  }
}

// List algorithms
trait ListImplementation[A] extends List[A] {

  override def head: Option[A] = this match {
    case h :: t => Some(h)
    case _ => None
  }
  override def tail: Option[List[A]] = this match {
    case h :: t => Some(t)
    case _ => None
  }
  override def append(list: List[A]): List[A] = this match {
    case h :: t => h :: (t append list)
    case _ => list
  }
  override def foreach(consumer: (A)=>Unit): Unit = this match {
    case h :: t => consumer(h); t foreach consumer
    case _ => None
  }
  override def get(pos: Int): Option[A] = this match {
    case h :: t if pos == 0 => Some(h)
    case h :: t if pos > 0 => t get (pos-1)
    case _ => None
  }
  override def filter(predicate: (A) => Boolean): List[A] = this match {
    case h :: t if (predicate(h)) => h :: (t filter predicate)
    case _ :: t => (t filter predicate)
    case _ => Nil()
  }
  override def map[B](fun: (A) => B): List[B] = this match {
    case h :: t => fun(h) :: (t map fun)
    case _ => Nil()
  }

  override def toSeq: Seq[A] = this match {
    case h :: t => h +: t.toSeq // using method '+:' in Seq..
    case _ => Seq()
  }

  override def foldLeft[B](acc: B)(f: (B,A)=>B): B = this match {
    case Cons(h,t) => t.foldLeft(f(acc,h))(f)
    case Nil() => acc
  }

  override def foldRight[B](acc: B)(f: (A, B) => B): B =
    this.reverse().foldLeft(acc)((acc,elem) => f(elem,acc))

  override def reverse(): List[A] =
    this.foldLeft(Nil[A]().asInstanceOf[List[A]])((acc, elem) => Cons(elem,acc))

  override def flatMap[B](f: A => List[B]): List[B] = this match {
    case Cons(h,t) => f(h).append(t.flatMap(f))
    case Nil() => Nil()
  }

  override def zipRight: List[(A,Int)] = {
    @tailrec
    def _zip(l: List[A], index: Int = 0, res: List[(A, Int)] = List.nil): List[(A, Int)] = l match {
      case h :: t => _zip(t, index + 1, (h, index) :: res)
      case _ => res
    }
    _zip(this).reverse()
  } // questions: what is the type of keyword ???

  override def partition(pred: A => Boolean): (List[A], List[A]) = {
    @tailrec
    def _partition(l: List[A])(pred: A => Boolean)(acc: (List[A], List[A]) = (List.nil, List.nil)): (List[A], List[A]) = l match {
      case h :: t if pred(h) => _partition(t)(pred)((h :: acc._1, acc._2))
      case h :: t if !pred(h) => _partition(t)(pred)((acc._1, h :: acc._2))
      case _ => (acc._1.reverse(), acc._2.reverse())
    }
    _partition(this)(pred)()
  }

  override def span(pred: A => Boolean): (List[A],List[A]) = {
    var split = false
    @tailrec
    def _span(l: List[A])(pred: A => Boolean)(res: (List[A], List[A]) = (List.nil, List.nil)): (List[A], List[A]) = (l, split) match {
      case (h :: t, false) if !pred(h) => _span(t)(pred)((h :: res._1, res._2))
      case (h :: t, false) if pred(h) => split = true; _span(t)(pred)((res._1, h :: res._2))
      case (h :: t, true) => _span(t)(pred)((res._1, h :: res._2))
      case _ => (res._1.reverse(), res._2.reverse())
    }
    _span(this)(pred)()
  }

  /**
    *
    * @throws UnsupportedOperationException if the list is empty
    */
  override def reduce(op: (A,A)=>A): A = {
    this.head match {
      case None => throw new UnsupportedOperationException()
      case Some(_) => this.tail.get.foldLeft(this.head.get)(op)
    }
  }

  override def takeRight(n: Int): List[A] = {
    @tailrec
    def _takeRight(l: List[A])(n: Int)(res: List[A] = List.nil): List[A] = l match {
      case h :: t if n > 0 => _takeRight(t)(n - 1)(h :: res)
      case _ => res
    }
    _takeRight(this.reverse())(n)()
  }

  override def collect[B](partial: PartialFunction[A, B]): List[B] = {
    @tailrec
    def _collect(l: List[A])(pf: PartialFunction[A, B])(res: List[B] = List.nil): List[B] = l match {
      case h :: t if pf.isDefinedAt(h) => _collect(t)(pf)(pf(h) :: res)
      case _ :: t => _collect(t)(pf)(res)
      case _  => res.reverse()
    }
    _collect(this)(partial)()
  }
}

// Factories
object List {

  // Smart constructors
  def nil[A]: List[A] = Nil()
  def cons[A](h: A, t: List[A]): List[A] = Cons(h,t)

  def apply[A](elems: A*): List[A] = {
    var list: List[A] = Nil()
    for (i <- elems.length-1 to 0 by -1) list = elems(i) :: list
    list
  }

  def of[A](elem: A, n: Int): List[A] =
    if (n==0) Nil() else elem :: of(elem,n-1)
}

object ListsTest extends App {

  import List._  // Working with the above lists
  println(List(10,20,30,40))
  val l = 10 :: 20 :: 30 :: 40 :: Nil() // same as above
  println(l.head) // 10
  println(l.tail) // 20,30,40
  println(l append l) // 10,20,30,40,10,20,30,40
  println(l append l toSeq) // as a list: 10,20,30,40,10,20,30,40
  println(l get 2) // 30
  println(of("a",10)) // a,a,a,..,a
  println(l filter (_<=20) map ("a"+_) ) // a10, a20

  assert(List(1,2,3) == List(1,2,3))

  println(scala.collection.immutable.List(10,20,30,40).partition(_>15))
  println(scala.collection.immutable.List(10,20,30,40).span(_>15))

  // Ex. 1: zipRight
  println(l.zipRight.toSeq) // List((10,0), (20,1), (30,2), (40,3))

  // Ex. 2: partition
  println(l.partition(_>15)) // ( Cons(20,Cons(30,Cons(40,Nil()))), Cons(10,Nil()) )

  // Ex. 3: span
  println(l.span(_>15)) // ( Nil(), Cons(10,Cons(20,Cons(30,Cons(40,Nil())))) )
  println(l.span(_<15)) // ( Cons(10,Nil()), Cons(20,Cons(30,Cons(40,Nil()))) )

  // Ex. 4: reduce
  println(l.reduce(_+_)) // 100
  println(List(10).reduce(_+_)) // 10
  try { List[Int]().reduce(_+_); assert(false) } catch { case _:UnsupportedOperationException => }

  // Ex. 5: takeRight
  println(l.takeRight(2)) // Cons(30,Cons(40,Nil()))

  // Ex. 6: collect
  println(l.collect { case x if x<15 || x>35 => x-1 }) // Cons(9, Cons(39, Nil()))
}