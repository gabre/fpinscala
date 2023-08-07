package fpinscala.testing

import fpinscala.state.RNG.{Rand, Simple}
import fpinscala.state.State

import java.util.Calendar
import scala.language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

case class Prop(bool: () => Boolean) {
  def assertTrue(): Unit = assert(bool())
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop(() => f(Gen.run(gen)))
}

object Gen {
  implicit def toGenOps[A](a: Gen[A]): GenOps[A] = new GenOps[A](a)

  def unit[A](a: => A): Gen[A] = Gen(State.unit(a))
  def combine[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] =
    a.flatMap(v1 => b.flatMap(v2 => unit((v1, v2))))

  def run[A](g: Gen[A]): A = {
    val now = Calendar.getInstance()
    g.r.run(Simple(now.getTimeInMillis))._1
  }

  val intGen: Gen[Int] = Gen(State(_.nextInt))
}

case class Gen[A](r: State.Rand[A]) {
  def map[B](f: A => B): Gen[B] = Gen(r.map(f))
  def flatMap[B](f: A => Gen[B]): Gen[B] = Gen(r.flatMap(a => f(a).r))
}


class GenOps[A] (val gen: Gen[A]) {
  def combine[B](b: Gen[B]): Gen[(A, B)] = Gen.combine(gen, b)
}

trait SGen[+A] {

}

