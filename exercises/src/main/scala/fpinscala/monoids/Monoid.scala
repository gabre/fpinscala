package fpinscala.monoids

import fpinscala.parallelism.Nonblocking._
import fpinscala.state.RNG

import language.higherKinds

trait Monoid[A] {
  def op(a1: A, a2: A): A
  def zero: A
}

object Monoid {

  def dual[A](m: Monoid[A]): Monoid[A] = new Monoid[A] {
    override def op(a1: A, a2: A): A = m.op(a2, a1)
    override def zero: A = m.zero
  }

  val stringMonoid = new Monoid[String] {
    def op(a1: String, a2: String) = a1 + a2
    val zero = ""
  }

  def listMonoid[A] = new Monoid[List[A]] {
    def op(a1: List[A], a2: List[A]) = a1 ++ a2
    val zero = Nil
  }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2
    override def zero: Boolean = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1 orElse a2
    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(a1: A => A, a2: A => A): A => A = a1 compose a2
    override def zero: A => A = identity
  }

  import fpinscala.testing._
  import Prop._
  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop = {
    forAll(gen combine gen combine gen) { abc =>
      val ((a, b), c) = abc
      a == m.op(a, m.zero)
      m.op(m.op(a, b), c) == m.op(a, m.op(b, c))
    }
  }

  def monoidLawsIntAddition: Prop = {
    monoidLaws(intAddition, Gen.intGen)
  }


  def trimMonoid(s: String): Monoid[String] = ???

  def concatenate[A](as: List[A], m: Monoid[A]): A =
    as.foldLeft(m.zero)((acc, item) => m.op(item, acc))

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.foldLeft(m.zero)((acc, item) => m.op(acc, f(item)))

  def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
    foldMap[A, B => B](as, endoMonoid)(a => f(a, _))(z)

  def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B =
    foldMap[A, B => B](as, dual(endoMonoid))(a => f(_, a))(z)

  // But if we have a monoid, we can reduce a list using a balanced fold,
  // which can be more efficient for some operations and also allows for parallelism.
  def foldMapV[A, B](as: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (as.length == 0) m.zero
    else if (as.length == 1) f(as(0))
    else {
      val middle = as.length / 2
      val (a, b) = as.splitAt(middle)
      m.op(foldMapV(a, m)(f), foldMapV(b, m)(f))
    }
  }

  def ordered(ints: IndexedSeq[Int]): Boolean =
    foldMapV(ints, new Monoid[(Int, Boolean, Int)] {
      override def op(a1: (Int, Boolean, Int), a2: (Int, Boolean, Int)): (Int, Boolean, Int) = (a1, a2) match {
        case ((n1, true, n2), (n3, true, n4)) if n2 <= n3
          => (n1, true, n4)
        case ((n1, _, n2), (n3, _, n4))
          => (n1, false, n4)
      }
      override def zero: (Int, Boolean, Int) = (Int.MaxValue, true, Int.MinValue)
    })(num => (num, true, num))._2

  sealed trait WC
  case class Stub(chars: String) extends WC
  case class Part(lStub: String, words: Int, rStub: String) extends WC

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    ???

  def parFoldMap[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
    ???

  val wcMonoid: Monoid[WC] = ???

  def count(s: String): Int = ???

  def productMonoid[A,B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] =
    new Monoid[(A, B)] {
      override def op(a1: (A, B), a2: (A, B)): (A, B) = (A.op(a1._1, a2._1), B.op(a1._2, a2._2))
      override def zero: (A, B) = (A.zero, B.zero)
    }

  def functionMonoid[A,B](B: Monoid[B]): Monoid[A => B] =
    new Monoid[A => B] {
      override def op(a1: A => B, a2: A => B): A => B = a => B.op(a1(a), a2(a))
      override def zero: A => B = a => B.zero
    }

  def mapMergeMonoid[K,V](V: Monoid[V]): Monoid[Map[K, V]] =
    new Monoid[Map[K, V]] {
      override def op(a1: Map[K, V], a2: Map[K, V]): Map[K, V] =
        Map.from (for (k <- a1.keySet.union(a2.keySet)) yield
          (a1.get(k), a2.get(k)) match {
            case (Some(v1), Some(v2)) => (k, V.op(v1, v2))
            case (Some(v1), None) => (k, v1)
            case (None, Some(v2)) => (k, v2)
          })
      override def zero: Map[K, V] = Map.empty
    }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    ???
}

trait Foldable[F[_]] {
  import Monoid._

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B =
    foldMap[A, B => B](as)((a: A) => f(a, _))(endoMonoid)(z)

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B =
    foldMap[A, B => B](as)((a: A) => f(_, a))(dual(endoMonoid))(z)

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldRight(as)(m.zero)((a1, a2) => m.op(a1, a2))

  def toList[A](as: F[A]): List[A] =
    foldRight(as)(List[A]())((a, b) => a :: b)
}

object ListFoldable extends Foldable[List] {
  override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: List[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
}

object IndexedSeqFoldable extends Foldable[IndexedSeq] {
  override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
  override def foldMap[A, B](as: IndexedSeq[A])(f: A => B)(mb: Monoid[B]): B =
    as.foldRight(mb.zero)((a, b) => mb.op(f(a), b))
}

object StreamFoldable extends Foldable[Stream] {
  override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B) =
    as.foldRight(z)(f)
  override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B) =
    as.foldLeft(z)(f)
}

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object TreeFoldable extends Foldable[Tree] {
  override def foldMap[A, B](as: Tree[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B) =
    as match {
      case Leaf(value) => f(z, value)
      case Branch(left, right) => foldLeft(right)(foldLeft(left)(z)(f))(f)
    }

  override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B) =
    as match {
      case Leaf(value) => f(value, z)
      case Branch(left, right) => foldRight(left)(foldRight(right)(z)(f))(f)
    }
}

object OptionFoldable extends Foldable[Option] {
  override def foldMap[A, B](as: Option[A])(f: A => B)(mb: Monoid[B]): B =
    foldRight(as)(mb.zero)((a, b) => mb.op(f(a), b))
  override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B) =
    as match {
      case Some(value) => f(z, value)
      case None => z
    }
  override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B) =
    foldLeft(as)(z)((b, a) => f(a, b))
}

object RunMonoid {
  def main(args: Array[String]): Unit = {
    assert(Monoid.ordered(IndexedSeq[Int]()))
    assert(Monoid.ordered(IndexedSeq[Int](1)))
    assert(Monoid.ordered(IndexedSeq[Int](2,4)))
    assert(Monoid.ordered(IndexedSeq[Int](1,3,5,6)))
    assert(!Monoid.ordered(IndexedSeq[Int](3,2)))
    assert(!Monoid.ordered(IndexedSeq[Int](2,1,5)))
  }
}