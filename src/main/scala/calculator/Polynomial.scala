package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    new Var[Double](b() * b() - 4 * a() * c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
      new Var[Set[Double]]({
        
        if(delta() < 0.0) Set()
        else{
          val sqrtDelta = math.sqrt(delta())
          val s1 =  (0.0 - b() - sqrtDelta)/(a() * 2.0)
          val s2 =  (0.0 - b() + sqrtDelta)/(a() * 2.0)
          Set(s1, s2)
        }
      }
    )
  }
}
