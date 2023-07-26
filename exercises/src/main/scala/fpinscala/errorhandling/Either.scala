package fpinscala.errorhandling


import scala.{Either => _, Left => _, Option => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter
import scala.collection.immutable.::
sealed trait Either[+E,+A] {
 def map[B](f: A => B): Either[E, B] = this match {
   case Right(r) => Right(f(r))
   case l => l.asInstanceOf[Either[E, B]]
 }

 def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
   case Left(e) => Left(e)
   case Right(r) => f(r)
 }

 def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
   case Left(get) => b
   case r => r
 }

 def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = (this, b) match {
   case (Right(r1), Right(r2)) => Right(f(r1, r2))
   case (Left(e), _) => Left(e)
   case (_, Left(e)) => Left(e)
 }
}
case class Left[+E](get: E) extends Either[E,Nothing]
case class Right[+A](get: A) extends Either[Nothing,A]

object Either {
  // HARD
  def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(List()))((nextElem, accEither) => f(nextElem).map2(accEither)(_ :: _))
  def traverse_2[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    es.foldRight[Either[E, List[B]]](Right(List()))(
      (nextElem, accEither) => for {
        acc <- accEither
        result <- f(nextElem)
      } yield scala.collection.immutable.::(result, acc)
    )

  def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List()))(
      (nextElem, accEither) => for {
        acc <- accEither
        result <- nextElem
      } yield scala.collection.immutable.::(result, acc)
    )

  def sequence_2[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    es.foldRight[Either[E, List[A]]](Right(List()))(
      (nextElem, accEither) =>
        accEither
          .flatMap(acc =>
            nextElem
              .flatMap(result =>
                Right(scala.collection.immutable.::(result, acc)))))

  def sequence_3[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse[E, Either[E, A], A](es)(identity)

  def mean(xs: IndexedSeq[Double]): Either[String, Double] = 
    if (xs.isEmpty) 
      Left("mean of empty list!")
    else 
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] = 
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

}