package week3.DigitalCircuits

/**
  * Created by shexiaogui on 10/04/17.
  */




abstract class Gates extends Simulation {
  def InverterDelay: Int
  def AndGateDelay: Int
  def OrGateDelay: Int
  
  class Wire {
    private var sigVal = false // current value of the signal
    private var actions: List[Action] = List()
    
    def getSignal: Boolean = sigVal
    def setSignal(s: Boolean): Unit =
      if(s != sigVal) {
        sigVal = s
        actions foreach (_ ()) // for(a <- actions) a()
      }
    def addActions(a: Action): Unit = {
      actions = a::actions
      a()
    }
  }
  
  def orGate(inputA: Wire, inputB: Wire, output: Wire): Unit =  {
    def orAction(): Unit = {
      val inSigA = inputA.getSignal
      val inSigB = inputB.getSignal
      afterDelay(OrGateDelay){output setSignal(inSigA || inSigB)}
    }
    inputA.addActions(orAction)
    inputB.addActions(orAction)// Once a input is changed, the action will be triggered
  }
  def orGateAlt(in1: Wire, in2: Wire, output: Wire): Unit = {
    val notIn1, notIn2, notOut = new Wire
    inverter(in1, notIn1)
    inverter(in2, notIn2)
    andGate(notIn1, notIn2, notOut)
    inverter(notOut, output)
  }
  
  def andGate(inputA: Wire, inputB: Wire, output: Wire): Unit = {
    def andAction(): Unit = {
      val inSigA = inputA.getSignal
      val inSigB = inputB.getSignal
      afterDelay(AndGateDelay){output setSignal(inSigA && inSigB)}
    }
    inputA.addActions(andAction)
    inputB.addActions(andAction)// Once a input is changed, the action will be triggered
  }
  def inverter(input: Wire, output: Wire): Unit = {
    def invertAction(): Unit = {
      val inputSig = input.getSignal
      afterDelay(InverterDelay){
        output.setSignal(!inputSig)
      }
    }
    input addActions(invertAction)
  }
  
  def probe(name: String, wire: Wire): Unit = {
    def probeAction(): Unit = {
      println(s"$name $currentTime value = ${wire.getSignal}")
    }
    wire addActions(probeAction)
  }
  
}
