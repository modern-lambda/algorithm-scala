package Collection

abstract sealed class List[+A] {
  def head: A

  def tail: List[A]

  def isEmpty: Boolean

  def append[B >: A](x: B): List[B] =
    if (isEmpty) List.make(x)
    else List.make(head, tail.append(x))

  def prepend[B >: A](x: B): List[B] = List.make(x, this)

  def concat[B >: A](xs: List[B]): List[B] =
    if (isEmpty) xs
    else tail.concat(xs).prepend(head)

  def remove[B >: A](x: B): List[B] =
    if (isEmpty) fail("Can't find " + x + " in this list.")
    else if (x != head) List.make(head, tail.remove(x))
    else tail

  def apply(n: Int): A =
    if (isEmpty) fail("Index out of bounds.")
    else if (n < 0) fail("Index (< 0> out of bounds")
    else if (n == 0) head
    else tail(n - 1)

  def contains[B >: A](x: B): Boolean =
    if (isEmpty) false
    else if (x != head) tail.contains(x)
    else true

  def foreach(f: (A) => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }

  def fold[B](n: B)(op: (B, A) => B): B = {
    def loop(l: List[A], a: B): B =
      if (l.isEmpty) a
      else loop(l.tail, op(a, l.head))

    loop(this, n)
  }

  def sum[B >: A](implicit num: Numeric[B]): B = fold(num.zero)(num.plus)

  def product[B >: A](implicit num: Numeric[B]): B = fold(num.one)(num.times)

  def min[B >: A](implicit ordering: Ordering[B]): B =
    if (isEmpty) fail("An empty list.")
    else if (tail.isEmpty) head
    else ordering.min(head, tail.min(ordering))

  def max[B >: A](implicit ordering: Ordering[B]): B =
    if (isEmpty) fail("An empty list.")
    else if (tail.isEmpty) head
    else ordering.max(head, tail.max(ordering))

  def slice(from: Int, until: Int): List[A] =
    if (isEmpty || until == 0) List.empty
    else if (from == 0) tail.slice(from, until - 1).prepend(head)
    else tail.slice(from - 1, until - 1)

  def reverse: List[A] = {
    def loop(s: List[A], d: List[A]): List[A] =
      if (s.isEmpty) d
      else loop(s.tail, d.prepend(s.head))

    loop(this, List.empty)
  }

  def length: Int =
    if (isEmpty) 0
    else 1 + tail.length

  override def toString: String = {
    def loop(h: A, t: List[A], s: String): String =
      if (!t.isEmpty) loop(t.head, t.tail, s + h + ", ")
      else s + h

    if (isEmpty) "List[]"
    else "List[" + loop(head, tail, "") + "]"
  }

  def fail(m: String) = throw new NoSuchElementException(m)
}

case object Nil extends List[Nothing] {
  def head: Nothing = fail("An empty list.")
  def tail: List[Nothing] = fail("An empty list.")

  def isEmpty: Boolean = true
}

case class Cons[A](head: A, tail: List[A]) extends List[A] {
  override def isEmpty: Boolean = false
}

object List {
  def empty[A]: List[A] = Nil

  def make[A](x: A, t: List[A] = Nil): List[A] = Cons(x, t)

  def apply[A](xs: A*): List[A] = {
    var r: List[A] = List.empty
    for (x <- xs.reverse) r = r.prepend(x)
    r
  }
}

/*
object Main extends App {
  var l = List(1, 2, 3, 4, 5)
  println(l)
}
*/