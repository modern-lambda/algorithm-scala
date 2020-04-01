package Collection

abstract class Stack[+A] {
  def top: A
  def rest: Stack[A]

  def isEmpty: Boolean

  def pop: (A, Stack[A]) = (top, rest)

  def push[B >: A](v: B): Stack[B] = new NonEmptyStack(v, this)
}

object NIL extends Stack[Nothing] {
  def top: Nothing = throw new NoSuchElementException("Empty stack.")
  def rest: Nothing = throw new NoSuchElementException("Empty stack.")

  def isEmpty: Boolean = true
}

class NonEmptyStack[A](t: A, r: Stack[A] = NIL) extends Stack[A] {
  def top: A = t

  def rest: Stack[A] = r

  def isEmpty: Boolean = false
}