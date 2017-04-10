package week3.DigitalCircuits



/**
  * Created by shexiaogui on 10/04/17.
  */



abstract class Simulation {
  type Action = () => Unit
  case class Event(time: Int, action: Action)
  private type Agenda = List[Event]
  private var agenda: Agenda = Nil
  private var curtime = 0
  def currentTime: Int = curtime
  
  def afterDelay(delay:Int)(block: => Unit): Unit = {
    val item = Event(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }
  
  private def insert(ag: List[Event], item: Event): List[Event] = ag match {
    case first :: rest if first.time <= item.time =>
      first :: insert(rest, item)
    case _ => item :: ag
  }
  
  private def loop(): Unit = agenda match {
    case first :: rest =>
      agenda = rest
      curtime = first.time
      first.action()
      loop()
    case Nil =>
  }
  
  def run(): Unit = {
    afterDelay(0){
      println("simulation starts at time " + currentTime + "...")
    }
    loop()
  }
  
  
}


