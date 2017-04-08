/**
  * Created by shexiaogui on 08/04/17.
  */
package week1

trait Generator [+T] {
  self =>
  def generate: T
  
  val integers = new Generator[Int] {
    val rand = new java.util.Random
    override def generate = rand.nextInt()
  }
  
  val booleans = integers.map(x => x > 0)
  
  
  def pair[T, U](tg: Generator[T], ug: Generator[U]): Generator[(T, U)] = tg.flatMap(t => ug.map(u => (t, u)))
  
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate = f(self.generate)
  }
  
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate = f(self.generate).generate
  }
  
  def single[T](x: T): Generator[T] = new Generator[T] {
    override def generate = x
  }
  
  def choose(lo: Int, hi: Int): Generator[Int] = for (x <- integers) yield lo + x % (hi - lo)
  
  def oneOf[T](xs: T*): Generator[T] = for (idx <- choose(0, xs.length)) yield xs(idx)
  
  def emptyList = single(Nil)
  
  def nonEmptyList = for {
    head <- integers
    tail <- lists
  } yield head :: tail
  
  def lists: Generator[List[Int]] = for {
    isEmpty <- booleans
    list <- if (isEmpty) emptyList else nonEmptyList
  } yield list
  
  def leaf: Generator[Leaf] = for{
    integer <- integers
  } yield Leaf(integer)
  
  def inner: Generator[Inner] = for{
    left <- trees
    right <- trees
  } yield Inner(left, right)
  
  def trees: Generator[Tree] = for{
    isLeaf <- booleans
    tree <- if(isLeaf) leaf else inner
  } yield tree
  
  def test[T](g: Generator[T], numTime: Int = 100)(f: T => Boolean): Unit = {
    for(i <- 0 until numTime){
      val value = g.generate
      assert(f(value), "test failed for value: " + value)
    }
    println("passed " + numTime + " tests")
  }
  
}








