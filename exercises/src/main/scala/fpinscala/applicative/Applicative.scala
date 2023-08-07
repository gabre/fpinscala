package fpinscala
package applicative

import monads.{Functor}
import state._
import monoids._

import language.higherKinds
import language.implicitConversions

trait Applicative[F[_]] extends Functor[F] {

  def map2[A,B,C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = {
    val fb2c = map(fa)(a => f(a, _))
    apply(fb2c)(fb)
  }

  def apply[A,B](fab: F[A => B])(fa: F[A]): F[B] =
    map2(fab, fa)((ab, a) => ab.apply(a))

  def unit[A](a: => A): F[A]

  def map[A,B](fa: F[A])(f: A => B): F[B] =
    apply(unit(f))(fa)

  def sequence[A](fas: List[F[A]]): F[List[A]] =
    fas.foldRight(unit(List[A]()))(((fa, facc) => map2(fa, facc)((a, acc) => a :: acc)))

  def sequence2[A](fas: List[F[A]]): F[List[A]] = traverse(fas)(identity)

  def traverse[A,B](as: List[A])(f: A => F[B]): F[List[B]] =
    as.foldRight(unit(List[B]()))(((a, facc) => map2(f(a), facc)((a, acc) => a :: acc)))

  def replicateM[A](n: Int, fa: F[A]): F[List[A]] =
    traverse((1 to n).toList)(_ => fa)

  def factor[A,B](fa: F[A], fb: F[B]): F[(A,B)] = map2(fa, fb)((a, b) => (a, b))

  def product[G[_]](G: Applicative[G]): Applicative[({type f[x] = (F[x], G[x])})#f] = {
    val F = this
    new Applicative[({type f[x] = (F[x], G[x])})#f] {
      override def unit[A](a: => A): (F[A], G[A]) = (F.unit(a), G.unit(a))
    }
  }

  def compose[G[_]](G: Applicative[G]): Applicative[({type f[x] = F[G[x]]})#f] = {
    val F = this
    new Applicative[({type f[x] = F[G[x]]})#f] {
      override def unit[A](a: => A): F[G[A]] = F.unit(G.unit(a))
    }
  }

  def sequenceMap[K,V](ofa: Map[K,F[V]]): F[Map[K,V]] =
    map(traverse(ofa.toList)(kv => map(kv._2)(v => (kv._1, v))))(_.toMap)
}

case class Tree[+A](head: A, tail: List[Tree[A]])

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A,B](ma: F[A])(f: A => F[B]): F[B] = join(map(ma)(f))

  def join[A](mma: F[F[A]]): F[A] = flatMap(mma)(ma => ma)

  def compose[A,B,C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A,B](mf: F[A => B])(ma: F[A]): F[B] =
    flatMap(mf)(f => map(ma)(a => f(a)))
}

object Monad {
  def eitherMonad[E]: Monad[({type f[x] = Either[E, x]})#f] =
    new Monad[({type f[x] = Either[E, x]})#f] {
      override def unit[A](a: => A): Either[E, A] = Right(a)

      override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
        ma match {
          case Left(e) => Left(e)
          case Right(a) => f(a)
        }
    }

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    def unit[A](a: => A): State[S, A] = State(s => (a, s))
    override def flatMap[A,B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f
  }

  def composeM[F[_],N[_]](implicit F: Monad[F], N: Monad[N], T: Traverse[N]):
    Monad[({type f[x] = F[N[x]]})#f] =
      new Monad[({type f[x] = F[N[x]]})#f] {
        override def unit[A](a: => A): F[N[A]] = F.unit(N.unit(a))

        override def flatMap[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
          F.flatMap[N[A], N[B]](ma)((na: N[A]) => F.map(T.sequence(N.map[A, F[N[B]]](na)(a => f(a))))(nnb => N.join(nnb)))

        def flatMap2[A, B](ma: F[N[A]])(f: A => F[N[B]]): F[N[B]] =
          F.flatMap[N[A], N[B]](ma)((na: N[A]) => F.map(T.traverse(na)(a => f(a)))(N.join))
    }
}

sealed trait Validation[+E, +A]

case class Failure[E](head: E, tail: Vector[E])
  extends Validation[E, Nothing]

case class Success[A](a: A) extends Validation[Nothing, A]


object Applicative {

  val streamApplicative = new Applicative[Stream] {

    def unit[A](a: => A): Stream[A] =
      Stream.continually(a) // The infinite, constant stream

    override def map2[A,B,C](a: Stream[A], b: Stream[B])( // Combine elements pointwise
                    f: (A,B) => C): Stream[C] =
      a zip b map f.tupled
  }

  def validationApplicative[E]: Applicative[({type f[x] = Validation[E,x]})#f] =
    new Applicative[({type f[x] = Validation[E, x]})#f] {
      override def unit[A](a: => A): Validation[E, A] = Success(a)

      override def apply[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] = {
        fab match {
          case f@Failure(_, _) => f
          case Success(fun) => fa match {
            case f@Failure(_, _) => f
            case Success(v) => Success(fun.apply(v))
          }
        }
      }

      def apply2[A, B](fab: Validation[E, A => B])(fa: Validation[E, A]): Validation[E, B] =
        map2(fab, fa)((fun, a) => fun.apply(a))
    }

  type Const[A, B] = A

  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({ type f[x] = Const[M, x] })#f] {
      def unit[A](a: => A): M = M.zero
      override def apply[A,B](m1: M)(m2: M): M = M.op(m1, m2)
    }
}

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[G[_]:Applicative,A,B](fa: F[A])(f: A => G[B]): G[F[B]] =
    sequence(map(fa)(f))
  def sequence[G[_]:Applicative,A](fma: F[G[A]]): G[F[A]] =
    traverse(fma)(ma => ma)

  type Id[A] = A
  val idMonad = new Monad[Id] {
    def unit[A](a: => A) = a

    override def flatMap[A, B](a: A)(f: A => B): B = f(a)
  }
  def map[A,B](fa: F[A])(f: A => B): F[B] = traverse[Id, A, B](fa)(a => f(a))(idMonad)

  import Applicative._

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B,x]})#f,A,Nothing](
      as)(f)(monoidApplicative(mb))

  def traverseS[S,A,B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S,x]})#f,A,B](fa)(f)(Monad.stateMonad)

  def mapAccum[S,A,B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => (for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _  <- State.set(s2)
    } yield b)).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def zipWithIndex[A](fa: F[A]): F[(A, Int)] =
    mapAccum(fa, 0)((a, s) => ((a, s), s + 1))._1

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, reversed) => (reversed.head, reversed.tail))._1

  override def foldLeft[A,B](fa: F[A])(z: B)(f: (B, A) => B): B =
    mapAccum(fa, z)((a, b) => (f(b, a), f(b, a)))._2

  def fuse[G[_],H[_],A,B](fa: F[A])(f: A => G[B], g: A => H[B])
                         (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    (traverse(fa)(f), traverse(fa)(g))

  def fuse_2[G[_], H[_], A, B](fa: F[A])(f: A => G[B], g: A => H[B])
                            (implicit G: Applicative[G], H: Applicative[H]): (G[F[B]], H[F[B]]) =
    traverse[({type f[x] = (G[x], H[x])})#f, A, B](fa)(a => (f(a), g(a)))(G product  H)

  // HARD
  def compose[G[_]](implicit G: Traverse[G]): Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[H[_] : Applicative, A, B](fa: F[G[A]])(f: A => H[B]): H[F[G[B]]] =
        self.traverse(fa)(a => G.traverse(a)(f))
    }
}

object Traverse {
  // I had to remove the Applicative restriction from the type parameter 'G'
  // The error message was: "can't existentially abstract over parameterized type G[B]"
  val listTraverse = new Traverse[List] {
    override def traverse[G[_], A, B](fa: List[A])(f: A => G[B])(implicit G: Applicative[G]): G[List[B]] =
      fa.foldRight(G.unit(List[B]()))((a, appAcc) => G.map2(f(a), appAcc)((b, acc) => b :: acc))

  }

  val optionTraverse = new Traverse[Option] {
    override def traverse[G[_], A, B](fa: Option[A])(f: A => G[B])(implicit G: Applicative[G]): G[Option[B]] =
      fa match {
        case Some(a) => G.map(f(a))(Some(_))
        case None => G.unit(None)
      }
  }

  val treeTraverse = new Traverse[Tree] {
    override def traverse[G[_], A, B](fa: Tree[A])(f: A => G[B])(implicit G: Applicative[G]): G[Tree[B]] =
      fa match {
        case Tree(node, rest) =>
          G.map2(
            f(node),
            listTraverse.traverse(rest)(tree => traverse(tree)(f)))(
            (a, b) => Tree(a, b))
      }

  }
}

// The `get` and `set` functions on `State` are used above,
// but aren't in the `exercises` subproject, so we include
// them here
object StateUtil {

  def get[S]: State[S, S] =
    State(s => (s, s))

  def set[S](s: S): State[S, Unit] =
    State(_ => ((), s))
}
