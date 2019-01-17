package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    element <- arbitrary[Int]
    heap <- oneOf(const(empty), genHeap)
  } yield insert(element, heap)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)


  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("B1") = forAll { _: Unit =>
    findMin(insert(8, insert(4, insert(1, empty)))) == 1
  }

  property("B2") = forAll(genHeap, arbitrary[Int]) { (h: H, x: Int) =>
    val min = if (findMin(h) < x) findMin(h) else x
    findMin(insert(min, h)) == min
  }


  property("B3") = forAll { (x: Int, y: Int) =>
    val max = if (x > y) x else y
    findMin(deleteMin(insert(y, insert(x, empty)))) == max
  }

  property("B4") = forAll { (xs: List[Int]) =>
    def removeAll(heap: H): List[Int] =
      if (isEmpty(heap)) Nil
      else {
        val min = findMin(heap)
        val h = deleteMin(heap)
        min :: removeAll(h)
      }
    def insertAll(xs: List[Int], heap: H): H = xs match {
      case Nil => empty
      case y :: ys =>
        insert(y, insertAll(ys, heap))
    }
    val h = insertAll(xs, empty)
    val ys = removeAll(h)
    xs.sorted == ys
  }


    property("B5") = forAll(genHeap, genHeap) { (h1: H, h2: H) =>
      val h1Min = findMin(h1)
      val h2Min = findMin(h2)
      val min = if (h1Min < h2Min) h1Min else h2Min
      findMin(meld(h1, h2)) == min
    }

}
