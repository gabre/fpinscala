package fpinscala.state

trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    map(_.nextInt)(math.abs)(rng)

  def double(rng: RNG): (Double, RNG) =
    map(nonNegativeInt)(i => i.toDouble / (i + 1))(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    map2(_.nextInt, double)(Function.untupled(identity))(rng)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    map(intDouble)(_.swap)(rng)

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence((1 to count).map(_ => (_: RNG).nextInt).toList)(rng)

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = sequence(fs)

  def sequence2[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def sequence3[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) =>
      flatMap(acc)(
        l => flatMap(f)(
          a => unit(a :: l))))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      (f(a), s2)
    }
  )

  def mapV2[B](f: A => B): State[S, B] = State(run.andThen(t => t.copy(_1 = f(t._1))))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
    s => {
      val (a, s2) = run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }
  )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, s2) = run(s)
      f(a).run(s2)
    }
  )
}

sealed trait Input


object State {
  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(s => ((), s))

  def unit[S, B](b: B): State[S, B] = State(s => (b, s))

  def modify[S, B](f: S => S): State[S, Unit] = State(s => ((), f(s)))

  def compose[S, A, B](state1: State[S, A], state2: State[S, B]): State[S, B] =
    State(s => {
      val (a, s2) = state1.run(s)
      state2.run(s2)
    })

  def flatten[S, A](list: List[State[S, A]]): State[S, Unit] =
    list.foldLeft(unit[S, Unit](Unit))((acc, s) => compose(acc, s).map(_ => Unit))
}

case object Coin extends Input

case object Turn extends Input

case class Machine(coinNeeded: Boolean, candies: Int, coins: Int)

// HARD
object Machine {
  def step(input: Input, machine: Machine): Machine = (machine, input) match {
    case (m@Machine(_, 0, _), Coin) => m
    case (Machine(true, candies, coins), Coin) => Machine(coinNeeded = false, candies, coins + 1)
    case (Machine(false, candies, coins), Turn) => Machine(coinNeeded = true, candies - 1, coins)
    case (m@Machine(false, _, _), Coin) => m
    case (m@Machine(true, _, _), Turn) => m
  }

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = State(machine => {
    val finalMachine = inputs.foldRight(machine)((input, machine) => step(input, machine))
    ((finalMachine.candies, finalMachine.coins), finalMachine)
  })

  def simulateMachine_v2(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.flatten[Machine, Unit](inputs.map(input => State.modify(step(input, _))))
      finalState <- State.get[Machine]
    } yield (finalState.candies, finalState.coins)
}