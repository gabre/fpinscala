package fpinscala
package monads

import fpinscala.parallelism.Par._
import fpinscala.parallelism._
import fpinscala.parsing._
import fpinscala.state._
import fpinscala.testing._

import scala.language.higherKinds


trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]

  def distribute[A,B](fab: F[(A, B)]): (F[A], F[B]) =
    (map(fab)(_._1), map(fab)(_._2))

  def codistribute[A,B](e: Either[F[A], F[B]]): F[Either[A, B]] = e match {
    case Left(fa) => map(fa)(Left(_))
    case Right(fb) => map(fb)(Right(_))
  }
}

object Functor {
  val listFunctor = new Functor[List] {
    def map[A,B](as: List[A])(f: A => B): List[B] = as map f
  }
}

trait Monad[M[_]] extends Functor[M] {
  def unit[A](a: => A): M[A]
  def flatMap[A,B](ma: M[A])(f: A => M[B]): M[B]

  def map[A,B](ma: M[A])(f: A => B): M[B] =
    flatMap(ma)(a => unit(f(a)))
  def map2[A,B,C](ma: M[A], mb: M[B])(f: (A, B) => C): M[C] =
    flatMap(ma)(a => map(mb)(b => f(a, b)))

  def sequence[A](lma: List[M[A]]): M[List[A]] =
    lma.foldRight[M[List[A]]](unit(List()))((m, macc) => flatMap(m)(i => flatMap(macc)(acc => unit(i :: acc))))

  def traverse[A,B](la: List[A])(f: A => M[B]): M[List[B]] =
    la.foldRight[M[List[B]]](unit(List()))((m, macc) => flatMap(f(m))(i => flatMap(macc)(acc => unit(i :: acc))))

  def replicateM[A](n: Int, ma: M[A]): M[List[A]] =
    sequence((1 to n).map(_ => ma).toList)

  def compose[A,B,C](f: A => M[B], g: B => M[C]): A => M[C] =
    a => flatMap(f(a))(b => g(b))

  // Implement in terms of `compose`:
  def _flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    compose[Unit,A,B]((_) => ma, a => f(a))(())

  def join[A](mma: M[M[A]]): M[A] =
    flatMap(mma)(identity)

  // Implement in terms of `join`:
  def __flatMap[A,B](ma: M[A])(f: A => M[B]): M[B] =
    join(map(ma)(a => f(a)))
}

case class Reader[R, A](run: R => A)

object Monad {
  val genMonad = new Monad[Gen] {
    def unit[A](a: => A): Gen[A] = Gen.unit(a)
    override def flatMap[A,B](ma: Gen[A])(f: A => Gen[B]): Gen[B] =
      ma flatMap f
  }

  val parMonad: Monad[Par] = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)
    override def flatMap[A, B](ma: Par[A])(f: A => Par[B]): Par[B] = flatMap(ma)(f)
  }

  def parserMonad[P[+_]](p: Parsers[P]): Monad[P] = ???

  val optionMonad: Monad[Option] = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Some(a)
    override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] = flatMap(ma)(f)
  }

  val streamMonad: Monad[Stream] = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] = unit(a)
    override def flatMap[A, B](ma: Stream[A])(f: A => Stream[B]): Stream[B] = flatMap(ma)(f)
  }

  val listMonad: Monad[List] = new Monad[List] {
    override def unit[A](a: => A): List[A] = unit(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = flatMap(ma)(f)
  }

  def stateMonad[S] = new Monad[({type f[x] = State[S,x]})#f] {
    def unit[A](a: => A): State[S, A] = unit(a)
    def flatMap[A, B](ma: State[S, A])(f: A => State[S, B]): State[S, B] = flatMap(ma)(f)
  }

  val idMonad: Monad[Id] = new Monad[Id] {
    override def unit[A](a: => A): Id[A] = new Id[A](a)

    override def flatMap[A, B](ma: Id[A])(f: A => Id[B]): Id[B] = ma.flatMap(a => f(a))
  }

  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R, A] = unit(a)
    def flatMap[A, B](ma: Reader[R, A])(f: A => Reader[R, B]): Reader[R, B] = flatMap(ma)(f)
  }
}

case class Id[A](value: A) {
  def map[B](f: A => B): Id[B] = Id(f(value))
  def flatMap[B](f: A => Id[B]): Id[B] = f(value)
}

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R,x]})#f] {
    def unit[A](a: => A): Reader[R,A] = new Reader[R, A](r => a)
    override def flatMap[A,B](st: Reader[R,A])(f: A => Reader[R,B]): Reader[R,B] =
      new Reader[R, B](r => f(st.run(r)).run(r))
  }
}

