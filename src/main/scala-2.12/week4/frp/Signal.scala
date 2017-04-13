package week4.frp


import scala.util.DynamicVariable

/**
  * Created by shexiaogui on 12/04/17.
  */
class Signal[T](expr: => T) {
  private var myExpr: () => T = _ // the myExpr and myValue are not initialized
  private var myValue: T = _
  private var observers: Set[Signal[_]] = Set()
  update(expr)
  
//  private val caller = new StackableVariable[Signal[_]](NoSignal)
  private val caller = new DynamicVariable[Signal[_]](NoSignal)// stored in global hash table by jdk
  
  protected def update(expr: => T): Unit = {
    myExpr = () => expr
    computeValue()
  }
  protected def computeValue(): Unit = {
    val newValue = caller.withValue(this)(myExpr())
    if(myValue != newValue){
      myValue = newValue
      val obs = observers
      observers = Set()
      obs.foreach(_.computeValue())
    }
  }
  def apply(): T = {
    observers += caller.value
    assert(!caller.value.observers.contains(this), "cyclic detected") // s() = s() + 1
    myValue
  }
}
object NoSignal extends Signal[Nothing](???){
  override def computeValue() = ()
}
object Signal{
  private val caller = new StackableVariable[Signal[_]](NoSignal)
//  def apply[T](expr: => T)= new Signal(expr)
}

class Var[T](expr: => T) extends Signal[T](expr){
  override def update(expr: => T): Unit = super.update(expr)
}
object Var{
  def apply[T](expr: => T) = new Var(expr)
}

class StackableVariable[T](init: T){
  private var values: List[T] = List(init) // stack
  def value: T = values.head
  def withValue[R](newValue: T)(op: => R): R = {
    values = value :: values
    try op finally values = values.tail
  }
}