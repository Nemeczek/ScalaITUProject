// Advanced Programming
// Andrzej Wasowski, IT University of Copenhagen

package fpinscala.laziness
import scala.language.higherKinds

import org.scalatest.FlatSpec
import org.scalatest.prop.Checkers
import org.scalacheck._
import org.scalacheck.Prop._
import Arbitrary.arbitrary

import scala.util.Random

// If you comment out all the import lines below, then you test the Scala
// Standard Library implementation of Streams. Interestingly, the standard
// library streams are stricter than those from the book, so some laziness tests
// fail on them :)

 import stream00._    // uncomment to test the book solution
// import stream01._ // uncomment to test the broken headOption implementation
// import stream02._ // uncomment to test another version that breaks headOption

class StreamSpecRLOCRADN extends FlatSpec with Checkers {

  import Stream._

  // headOption ----------------------------------------
  behavior of "headOption"

  // a scenario test:
  it should "return None on an empty Stream (01)" in {
    assert(empty.headOption == None)
  }

  // An example generator of random finite non-empty streams
  def list2stream[A] (la :List[A]): Stream[A] = la.foldRight (empty[A]) (cons[A](_,_))

  // In ScalaTest we use the check method to switch to ScalaCheck's internal DSL
  def genNonEmptyStream[A] (implicit arbA :Arbitrary[A]) :Gen[Stream[A]] =
    for { la <- arbitrary[List[A]] suchThat (_.nonEmpty)}
    yield list2stream (la)

  // a property test:
  it should "return the head of the stream packaged in Some (02)" in check {
    // the implict makes the generator available in the context
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    ("singleton" |:
      Prop.forAll { (n :Int) => cons (n,empty).headOption == Some (n) } ) &&
    ("random" |:
      Prop.forAll { (s :Stream[Int]) => s.headOption != None } )

  }

  // a property test:
  it should "not force the tail of the stream (03)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    "head" |:
      Prop.forAll { (n :Int) => cons(n, empty).headOption.contains(n) }
  }

  // take ----------------------------------------
  behavior of "take"

  it should "not force any heads nor any tails of the Stream it manipulates and not force (n+1)st head (force) (01 + 02)" in check {
    Prop.forAll { (n :Int) => {
      def intStream:Stream[Int] = cons(throw new AssertionError, throw new AssertionError)
      intStream.take(n)
      true
    }}
  }

  it should "always be true that s.take(n).take(n) == s.take(n) for any Stream s and any n (03)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    "idempotency" |:
      Prop.forAll { (n:Int, s:Stream[Int]) => s.take(n).take(n).toList == s.take(n).toList }
  }

  // drop ----------------------------------------
  behavior of "drop"

  it should "always be true that s.drop(n).drop(m) == s.drop(n+m) for any n, m (01)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    implicit def arbInt = Arbitrary[Int] (Gen.choose(0,1000))
   // implicit def arbIntM = Arbitrary[Int] (Gen.choose(0,Int.MaxValue))
      "additivity" |:
      Prop.forAll { (n: Int, m:Int, s:Stream[Int]) => s.drop(n).drop(m).toList == s.drop(n+m).toList }
  }

  // the below test is expected to fail, if a call to 'drop' is not lazy
  it should "hold that s.drop(n) does not force any of the dropped elements heads (02)" in check {
    Prop.forAll { (n :Int) => {
      def intStream:Stream[Int] = cons(throw new AssertionError(), intStream)
      intStream.drop(n%1000) // n mod 1000 --> handling of large/negative inputs
      true
    }}
  }

  // map
  behavior of "map"

  it should "hold that x.map(id) == x (01)" in check {
    implicit def arbIntStream = Arbitrary[Stream[Int]] (genNonEmptyStream[Int])
    Prop.forAll { (s:Stream[Int]) => s.map(x=>x).take(math.abs(Random.nextInt)).toList.zip(s.take(math.abs(Random.nextInt)).toList).forall{
      case(x,y) => x == y }
    }
  }

  // TODO  map (02) ??? It will crash ???

  // append ----------------------------------------
  behavior of "append"

  // TODO Propose properties yourself ...

}
