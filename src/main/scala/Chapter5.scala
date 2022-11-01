

object Chapter5 extends App {

  // Exercise 5.1
  def streamToList[A](stream: Stream[A]): List[A] =
    stream match {
      case Empty => List.empty
      case Cons(a, tail) => List(a.apply) ++ streamToList(tail.apply)
    }

  // Exercise 5.2
//  def take[A](stream: Stream[A], n: Int): List[A] = {
//    def go(index: Int, takenSoFar: List[A], restOfStream: Stream[A]): List[A] =
//      if (index < n)
//        restOfStream match {
//          case Empty => takenSoFar
//          case Cons(a, tail) =>  go(index+1, takenSoFar ::: List(a.apply), tail.apply())
//        }
//      else
//        takenSoFar
//
//    go(0, List.empty, stream)
//  }

  // take, return stream version
//  def take[A](stream: Stream[A], n: Int): Stream[A] = {
//    def go(index: Int, takenSoFar: Stream[A], restOfStream: Stream[A]): Stream[A] =
//      if (index < n)
//        restOfStream match {
//          case Empty => takenSoFar
//          case Cons(a, tail) => go(index+1, Stream.cons(a.apply, takenSoFar), tail.apply())
//        }
//      else
//        takenSoFar
//
//    go(0, Stream.empty, stream)
//  }



//  def drop[A](stream: Stream[A], n: Int): List[A] = {
//    def go(index: Int, takenSoFar: List[A], restOfStream: Stream[A]): List[A] = {
//      println(s"index: $index")
//      if (index < n)
//        restOfStream match {
//          case Empty => takenSoFar
//          case Cons(a, tail) => go(index+1, takenSoFar, tail.apply())
//        }
//      else
//        restOfStream match {
//          case Empty => takenSoFar
//          case Cons(a, tail) => go(index+1, takenSoFar ::: List(a.apply), tail.apply())
//        }
//    }
//
//    go(0, List.empty, stream)
//  }



  // Exercise 5.3
//  def takeWhile[A](stream: Stream[A], p: A => Boolean): Stream[A] =
//    stream match {
//      case Empty => Stream.empty
//      case Cons(a, tail) => if (p(a.apply))
//        Stream.cons(a.apply, takeWhile(tail.apply(), p))
//    }

//  def takeWhile[A](stream: Stream[A], p: A => Boolean): Stream[A] = {
//    def go(takenSoFar: Stream[A], restOfStream: Stream[A]): Stream[A] =
//      restOfStream match {
//        case Empty => Stream.empty
//        case Cons(a, tail) =>
//          if (p(a.apply))
//            go(Stream.cons[A](a.apply(), takenSoFar), tail.apply())
//          else
//            takenSoFar
//      }
//    go(Stream.empty, stream)
//  }


//    @annotation.tailrec
//    def foldRight[A, B](z: => B)(f: (A, => B) => B): B =
//      this match {
//        case Cons(h,t) => f(h(), t().foldRight(z)(f))
//        case _ => z
//      }




  //  override def run(args: List[String]): IO[ExitCode] = {
//    println(s"Hello Chapter 15: Threads and Locks")
//
//    IO(ExitCode.Success)
//  }

  def run(): Unit = {
    // 5.1
    val stream: Stream[String] = Cons(() => "hello", () => Stream("world", "mars", "venus"))
    val list: List[String] = streamToList(stream)
    println(s"stream to list output for strings: $list")

    val numberStream: Stream[Int] = Cons(() => 1, () => Stream(2,3,4,5,6,7))
    val numberList: List[Int] = streamToList(numberStream)
    println(s"stream to list output for numbers: $numberList")

    def square(n: Int): Int = n*n
    val numberStream2: Stream[Int] = Cons(() => 1, () => Stream(2,3,4,5,6,7))
    val numberList2: List[Int] = streamToList(numberStream2).map(square)
    println(s"stream to list output for numbers2: $numberList2")

    //5.2 take
    val first5 = numberStream.take(5)
    println(s"taken the first5 of numberStream: $first5")

    // 5.2 drop
    val skip3 = numberStream.drop(3)
    println(s"skip the first 3 of numberStream: $skip3")

    // 5.3 takeWhile
    def lessThan10(a: Int): Boolean = a < 10
    val takeWhileLessThan10 = numberStream.takeWhile(lessThan10)
    println(s"take while numbers are less than 10: ${streamToList(takeWhileLessThan10)}")

    def lessThan5(a: Int): Boolean = a < 5
    val takeWhileLessThan5 = numberStream.takeWhile(lessThan5)
    println(s"take while numbers are less than 5: ${streamToList(takeWhileLessThan5)}")

    val res = Stream[Int](1,2,3).take(2)
    println(s"Stream(1,2,3).take(2).toList: ${streamToList(res)}")


    // 5.4 forAll
    val forAllTrue = numberStream.forAll(_ < 100) // true
    val forAllFalse = numberStream.forAll(_ > 100) // false

    println(s"forAll... should be true: $forAllTrue")
    println(s"forAll... should be false: $forAllFalse")

    // 5.5 takeWhile2
    val takeWhile2a = Stream[Int](1,2,3,4,5,6,7).takeWhile2(_ < 5) // 1,2,3,4
    val takeWhile2b = Stream[Int](1,2,3,4,5,6,7).takeWhile2(_ < 100) // all 7 numbers
    val takeWhile2c = Stream[Int](10,9,8,7,6,5).takeWhile2(_ < 100) // all 6 numbers

    println(s"takeWhile2a should just be the first 4 numbers: ${takeWhile2a.toList}")
    println(s"takeWhile2b should be all 7 numbers: ${takeWhile2b.toList}")
    println(s"takeWhile2c should maybe return all 6 numbers: ${takeWhile2c.toList}") // nope empty list

    // 5.6 headOption
    println(s"Some head should be returned: ${numberStream.headOption2}")
    println(s"None should be returned: ${Stream().headOption2}")

    // 5.7
    // map
    val mapRes1 = streamToList(Stream(1,2,3,4,5,6,7).map(_ + 10))
    println(s"map should add 10 onto all numbers: $mapRes1")
    // filter
    val filterRes1 = streamToList(Stream(1,2,3,4,5,6,7).filter(_ % 2 == 0))
    println(s"filter should keep even numbers only: $filterRes1")
    val filterRes2 = streamToList(Stream(1,2,3,4,5,6,7).filter(_ % 2 != 0))
    println(s"filter should keep odd numbers only: $filterRes2")
    // append
    val appendRes1 = streamToList(Stream(1,2,3,4,5,6,7).myAppend(8))
    println(s"myAppend should add the 8 on to the end of the Stream: $appendRes1")
    val appendRes2 = streamToList(Stream(1,2,3,4,5,6,7).append(Stream(8)))
    println(s"append should add the 8 on to the end of the Stream: $appendRes2")
    // flatMap
    def add10ToStream(n: Int): Stream[Int] = Stream(n + 10)
    val flatMapRes = streamToList(Stream(1,2,3,4,5).flatMap(add10ToStream))
    println(s"flatMap should work!!! $flatMapRes")

    // 5.8
    //println(s"A load of yes's: ${Stream.constant("yes")}")
    //val ones: Stream[Int] = Stream.cons(1, ones)
    //println(s"A load of 1's: ${ones.toList}")

    // 5.10
    println(s"finite fibs... given 7: ${Stream.finiteFibs(7).toList}")
    println(s"finiteFibsNoGo ... given 7: ${Stream.finiteFibsNoGo(0,1,7,Stream.empty).toList}")

    // 5.11
    sealed trait OddOrEven
    case object Odd extends OddOrEven
    case object Even extends OddOrEven
    val initialState = Odd
    def nextStateProducer(oddOrEven: OddOrEven): Option[(Int, OddOrEven)] =
      oddOrEven match {
        case Odd => Some(1, Odd)
        case Even => Some(8, Even)
      }

    println(s"unfold something .... ${Stream.finiteUnfold[Int, OddOrEven](initialState)(nextStateProducer)(3).toList}")

  }

  run()

}