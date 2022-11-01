//sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

trait Stream[+A] {

  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

//  def toList[A]: List[A] = {
//    @annotation.tailrec
//    def go[A](ll: Stream[A], acc: List[A]): List[A] = ll match {
//      case Cons(h, t) => go(t(), h() :: acc)
//      case Empty => acc.reverse
//    }
//    go(this.apply(), Nil)
//  }

  def toList: List[A] = {
    @annotation.tailrec
    def go(s: Stream[A], acc: List[A]): List[A] = s match {
      case Cons(h, t) => go(t(), h() :: acc)
      case _ => acc
    }

    go(this, List()).reverse
  }


  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n-1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t() drop n-1
    case _ => this
  }

  def takeWhile(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => cons(h(), t() takeWhile f)
    case _ => empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) if (p(h())) => t().forAll(p)
      case Cons(h, _) if (!p(h())) => false
      case _ => true
    }

  def takeWhile2(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if(f(a)) cons(a, b) else empty)

  def headOption: Option[A] =
    this match {
      case Cons(h, _) => Some(h())
      case _ => None
    }

  def headOption2: Option[A] =
    foldRight[Option[A]](None)((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if(f(h)) cons(h, t) else t)

  // my attempt - very different to blue book answer below but it still works - I just had to copy the typing and use B
  def myAppend[B>:A](b: B): Stream[B] =
    foldRight(cons(b, empty[A]))((h, t) =>
      if (t == empty) cons(h, cons(b, empty[A]))
      else cons(h, t)
    )

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

//  def flatMap[C[_], B](s: => Stream[C[A]], f: A => B): Stream[C[A]] =
//    foldRight(empty[C[A]])((h,t) => cons(f(h), t map(f())))

//  def flatMap[C[A], B](s: => Stream[C[A]], f: A => B): Stream[C[B]] =
//    foldRight(empty[C[B]])(
//      (h, list2) => cons(f(h), list2 foldRight(empty[C[B]])((a, tail) => cons(f(a), map(f(tail)))))
//    )
// list2 is Stream[C[A]]

  def flatMap[B](f: (A) ⇒ Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  // 5.8
//  def constant[A](a: A): Stream[A] = {
//    val as: Stream[A] = Stream.cons(a, as)
//    as
//  }

  def constant2[A](a: A): Stream[A] =
    cons(a, constant2(a)) // ???

  // 5.9
  def from(n: Int): Stream[Int] = {
    def go(current: Int, numbersSoFar: Stream[Int]): Stream[Int] =
      go(current+1, Stream.cons(current, numbersSoFar))

    go(n, Stream.cons(n, Stream.empty[Int]))
  }

  def from2(n: Int): Stream[Int] =
    cons(n, from2(n+1))

  // 5.10
  def fibs: Stream[Int] = {
    def go(num1: Int, num2: Int): Stream[Int] =
      cons(num1, go(num2, num1 + num2))

    go(0, 1)
  }

  def finiteFibs(to: Int): Stream[Int] = {
    def go(current: Int, next: Int, streamSoFar: Stream[Int]): Stream[Int] = {
      if (next < to)
        go(next, current+next, cons(next, streamSoFar))
      else
        cons(next, streamSoFar)
    }
    go(0, 1, Stream.empty)
  }

  //let's try getting rid of the go function
  def finiteFibsNoGo(current: Int, next: Int, to: Int, streamSoFar: Stream[Int]): Stream[Int] =
    if(to != 0)
      finiteFibsNoGo(next, current+next, to-1, cons(next, streamSoFar))
    else
      streamSoFar

  // 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => Stream.empty
    }

  def finiteUnfold[A, S](z: S)(f: S => Option[(A, S)])(current: Int): Stream[A] = {
    if (current < 3)
        f(z) match {
          case Some((a, s)) => cons(a, finiteUnfold(s)(f)(current+1))
          case None => Stream.empty
        }
    else
      f(z) match {
        case Some((a, s)) => cons(a, Stream.empty)
        case None => Stream.empty
      }
  }


}


//import Stream._
object Stream extends Stream[String]