package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for{
      x <- arbitrary[Int]
      h <- oneOf(const(empty), genHeap)
    } yield insert(x, h)
  )
  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("min1") = forAll{a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }
  
  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }
  
  property("min insert 2 int") = forAll{(a: Int, b: Int) =>
    if(a != b){
      var h = insert(a, empty)
      h = insert(b, h)
      findMin(h) == math.min(a, b)
      val newh = deleteMin(h)
      findMin(newh) == math.max(a, b)
    }
    else true
   
  }
  
  property("insert and delete") =forAll{a: Int =>
    
    val h = insert(a, empty)
    val h1 = deleteMin(h)
    isEmpty(h1)
  }
  
  property("sorted") = forAll{list: List[Int] =>
          if(!list.isEmpty){
            var h = empty
            val sortedList = list.sorted(ord = Ordering[Int])
            for (x <- list) h = insert(x, h)
            val resultList = offer(h)
            sortedList.length == resultList.length && sortedList.zip(resultList).forall( pair => pair._1 == pair._2)
          }else true
  }
  
  def offer(heap: H): List[Int]  =  isEmpty(heap) match {
    case true => Nil
    case false => {
      val newh = deleteMin(heap)
      if(isEmpty(newh)) List(findMin(heap))
      else findMin(heap) :: offer(newh)
    }
      
  }
    
  
}
