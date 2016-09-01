package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._
import Math._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    x <- arbitrary[Int]
    h <- oneOf(Gen.const(empty), genHeap)
  } yield insert(x, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(a, insert(b, empty))
    findMin(h) == min(a, b)
  }

  property("empty") = forAll { a: Int =>
    val h = insert(a, empty)
    deleteMin(h) == empty
  }

  property("order1") = forAll { (h: H) =>
    def f(t: H, l: List[Int]): List[Int] = {
      if (isEmpty(t)) l
      else findMin(t) :: f(deleteMin(t), l)
    }
    val mins = f(h, Nil)
    mins == mins.sorted
  }

  property("order2") = forAll { (h1: H, h2: H)  =>
    def f(t: H, l: List[Int]): List[Int] = {
      if (isEmpty(t)) l
      else findMin(t) :: f(deleteMin(t), l)
    }
    val m1 = meld(h1, h2)
    val min1 = findMin(h1)
    val m2 = meld(deleteMin(h1), insert(min1, h2))
    val mins1 = f(m1, Nil)
    val mins2 = f(m2, Nil)
    mins1 == mins2
  }

  property("meld2") = forAll { (h1: H, h2: H) =>
    val min1 = findMin(h1)
    val min2 = findMin(h2)
    val m = meld(h1, h2)
    findMin(m) == min(min1, min2)
  }
}
