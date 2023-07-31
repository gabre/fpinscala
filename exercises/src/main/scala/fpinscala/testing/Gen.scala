package fpinscala.testing

import scala.language.implicitConversions

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/

trait Prop {
}

object Prop {
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = ???
}

object Gen {
  implicit def toGenOps[A](a: Gen[A]): GenOps[A] = new GenOps[A](a)

  def unit[A](a: => A): Gen[A] = ???
  def combine[A, B](a: Gen[A], b: Gen[B]): Gen[(A, B)] = ???
}

trait Gen[A] {
  def map[A,B](f: A => B): Gen[B] = ???
  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
}


class GenOps[A] (val gen: Gen[A]) {
  def combine[B](b: Gen[B]): Gen[(A, B)] = Gen.combine(gen, b)
}

trait SGen[+A] {

}

