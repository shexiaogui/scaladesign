/**
  * Created by shexiaogui on 08/04/17.
  */
package week1
trait Tree {
}

case class Inner(left: Tree, right:Tree) extends Tree
case class Leaf(x: Int) extends Tree
