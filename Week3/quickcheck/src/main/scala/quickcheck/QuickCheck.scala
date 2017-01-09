package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

/* Assignment description: We are given multiple implenentations of "Heaps" and we have to write
 * "properties" that will be used by ScalaCheck to check these heaps. ScalaCheck generates Random
 * inputs and tests the properties against these Heap implementations. When all tests defined in 
 * QuickCheckSuite.scala are successfull, the output will tell that which implementation satisy all 
 * the properties and which don't. The one which satisfies all is the right implementation.
 * */
abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    a <- arbitrary[Int]
    h <- oneOf(const(empty), genHeap)
  } yield insert(a, h)

  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  /* If you insert an element into an empty heap, 
   * finding the minimum of the resulting heap should return the inserted element.
   */
  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  /* If you insert any two elements into an empty heap, 
   * finding the minimum of the resulting heap should get the smallest of the two elements back.
   */
  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (List(a, b).min)
  }

  /* If you insert an element into an empty heap, then delete 
   * the minimum, the resulting heap should be empty.
   */
  property("min3") = forAll { (a: Int) =>
    val h = deleteMin(insert(a, empty))
    isEmpty(h) == true
  }

  /* Given any heap, you should get a sorted sequence of elements when continually 
   * finding and deleting minima. (Hint: recursion and helper functions are your friends.)
   */
  property("minRemSort") = forAll { (h: H) =>
    // Get the mins from the heap into a list one by one
    def minsList(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: minsList(deleteMin(h))
    }
    val list = minsList(h)
    list == list.sorted
  }

  /* Finding a minimum of the melding of any two heaps should return 
   * a minimum of one or the other.
   */
  property("minAfterMeld") = forAll { (h1: H, h2: H) =>
    val h = meld(h1, h2)
    val minh1 = findMin(h1)
    val minh2 = findMin(h2)
    val minh = findMin(h)
    (minh == minh1) || (minh == minh2)
  }

  /* Take 2 lists. Join them, and get the list of minimums as in "minRemSort".
   * Then Remove min from one list and push it into another. Meld the resulting lists, and again
   * get the list of minimums as in "minRemSort". 
   * Compare the 2 list of minimums, should be the same
   */
  property("minRemSortWithMeld") = forAll { (h1: H, h2: H) =>
    // Get the mins from the heap into a list one by one
    def minsList(h: H): List[Int] = {
      if (isEmpty(h)) Nil
      else findMin(h) :: minsList(deleteMin(h))
    }
    val meld1 = meld(h1, h2)
    val minsList1 = minsList(meld1)
    val min1 = findMin(h1)
    val meld2 = meld(deleteMin(h1), insert(min1, h2))
    val minsList2 = minsList(meld2)
    minsList1 == minsList2
  }

}
