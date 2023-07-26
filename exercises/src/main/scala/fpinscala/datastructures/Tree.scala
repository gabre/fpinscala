package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  /*
  We're using the method `max` that exists on all `Int` values rather than an explicit `if` expression.

  Note how similar the implementation is to `size`. We'll abstract out the common pattern in a later exercise.
  */
  private def maximum(t: Tree[Int]): Int = t match {
    case Leaf(value) => value
    case Branch(left, right) => math.max(maximum(left), maximum(right))
  }

  /*
  Again, note how similar the implementation is to `size` and `maximum`.
  */
  private def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + scala.math.max(depth(left), depth(right))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(Function.uncurried(map[A, B] _)(left, f), map(right)(f)) // just to try the curried form..
  }

  /*
  Like `foldRight` for lists, `fold` receives a "handler" for each of the data constructors of the type, and recursively
  accumulates some value using these handlers. As with `foldRight`, `fold(t)(Leaf(_))(Branch(_,_)) == t`, and we can use
  this function to implement just about any recursive function that would otherwise be defined by pattern matching.
  */
  private def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }

  def sizeViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((leftSize: Int, rightSize: Int) => 1 + leftSize + rightSize)

  def maximumViaFold(t: Tree[Int]): Int =
    fold(t)(identity)((leftMax: Int, rightMax: Int) => math.max(leftMax, rightMax))

  def depthViaFold[A](t: Tree[A]): Int =
    fold(t)(_ => 1)((leftDepth: Int, rightDepth: Int) => 1 + math.max(leftDepth, rightDepth))

  /*
  Note the type annotation required on the expression `Leaf(f(a))`. Without this annotation, we get an error like this:

  type mismatch;
    found   : fpinscala.datastructures.Branch[B]
    required: fpinscala.datastructures.Leaf[B]
       fold(t)(a => Leaf(f(a)))(Branch(_,_))
                                      ^

  This error is an unfortunate consequence of Scala using subtyping to encode algebraic data types. Without the
  annotation, the result type of the fold gets inferred as `Leaf[B]` and it is then expected that the second argument
  to `fold` will return `Leaf[B]`, which it doesn't (it returns `Branch[B]`). Really, we'd prefer Scala to
  infer `Tree[B]` as the result type in both cases. When working with algebraic data types in Scala, it's somewhat
  common to define helper functions that simply call the corresponding data constructors but give the less specific
  result type:

    def leaf[A](a: A): Tree[A] = Leaf(a)
    def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
  */
  def mapViaFold[A, B](t: Tree[A])(f: A => B): Tree[B] =
    fold[A, Tree[B]](t)(value => Leaf(f(value)))((left, right) => Branch(left, right))
}