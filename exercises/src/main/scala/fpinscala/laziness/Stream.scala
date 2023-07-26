package fpinscala.laziness

import Stream._

import scala.annotation.tailrec
trait Stream[+A] {
  @annotation.tailrec
  final def foldLeft[B](z: => B)(f: (=> B, A) => B): B =
    this match {
      case Cons(h, t) => t().foldLeft(f(z, h()))(f)
      case _ => z
    }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def takeFromRight(n: Int): Stream[A] = foldRight[(Int, Stream[A])]((n, Empty))(
    (item, iAndAcc) =>
      if (iAndAcc._1 <= 0) iAndAcc
      else (iAndAcc._1 - 1, Cons(() => item, () => iAndAcc._2)))._2

  def take(n: Int): Stream[A] = this match {
    case Cons(_, _) if n <= 0 => Empty
    case Cons(h, t) => Cons(h, () => t().take(n-1))
    case Empty => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, _) if !p(h()) => Empty
    case Cons(h, t) => Cons(h, () => t().takeWhile(p))
    case Empty => Empty
  }

  def forAll(p: A => Boolean): Boolean = foldRight(true)((item, result) => p(item) && result)

  def headOption: Option[A] = this match {
    case Cons(h, t) => Option(h())
    case Empty => Option.empty
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.
  def append[B >: A](other: Stream[B]): Stream[B] =
    foldRight(other)((item, acc) => cons(item, acc))

//  def appendSimilar[B <: A](other: Stream[B]): Stream[A] =
//    foldRight(other.asInstanceOf[Stream[A]])((item, acc) => cons(item, acc))

//  def appendSame(other: Stream[A]): Stream[A] =
//    foldRight(other)((item, acc) => cons(item, acc))

  def map[B](f: A => B): Stream[B] =
    foldRight[Stream[B]](Empty)((item, acc) => cons(f(item), acc))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight[Stream[B]](Empty)((item, acc) => f(item).append(acc))

  def filter(f: A => Boolean): Stream[A] =
    foldRight[Stream[A]](Empty)((item, acc) => if (f(item)) cons(item, acc) else acc)

  def startsWith[B](s: Stream[B]): Boolean = (this, s) match {
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWith(t2())
    case (_, Empty) => true
    case _ => false
  }

  override def toString: String = foldLeft[StringBuilder](new StringBuilder(">"))((stringBuilder, item) => stringBuilder.append(s"--> ${item}")).mkString
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] = Stream.cons(n, from(n))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((item, z2)) => cons(item, unfold(z2)(f))
    case None => Empty
  }
}

object TestStream {

  import Stream._

  def main(args: Array[String]): Unit = {
    1 :: List(2)
    println(s"${Stream(1,2,3,4).take(0)}")
    println(s"${Stream(1,2,3,4).take(1)}")
    println(s"${Stream(1,2,3,4).take(2)}")
    println(s"${Stream(1,2,3,4).take(3)}")
    println(s"${Stream(1,2,3,4).take(4)}")
    println(s"${Stream(1,2,3,4).take(5)}")

    println("-----------------")

    println(s"${Stream(1,2,3,4).takeWhile(i => i < 3)}")

    println("-----------------")

    println(s"${Stream().startsWith(Stream("a"))}")
    println(s"${Stream(1, 2, 3, 4).startsWith(Stream("a"))}")
    println(s"${Stream(1, 2, 3, 4).startsWith(Stream(1, 3, 4))}")
    println(s"${Stream(1, 2, 3, 4).startsWith(Stream())}")
    println(s"${Stream(1, 2, 3, 4).startsWith(Stream(1, 2))}")
    println(s"${Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4))}")
    println(s"${Stream(1, 2, 3, 4).startsWith(Stream(1, 2, 3, 4, 5))}")

  }
}