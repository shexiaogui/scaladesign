package week2

/**
  * Created by shexiaogui on 08/04/17.
  */
class Pouring(capacity: Vector[Int]) {
  //state
  type State = Vector[Int]
  val initialState = capacity.map(x => 0)
  
  //moves
  trait Move{
    def change(state: State): State
  }
  case class Empty(glass: Int) extends Move {
    override def change(state: State) = state.updated(glass, 0)
  }
  case class Fill(glass: Int) extends Move {
    override def change(state: State) = state.updated(glass, capacity(glass))
  }
  case class Pour(from: Int, to: Int) extends Move{
    override def change(state: State) = {
      val amount = state(from) min (capacity(to) - state(to))
      state.updated(from, state(from) - amount).updated(to, state(to) + amount)
    }
  }
  // all glasses marked with number
  val glasses = 0 until capacity.length
  // all possible moves
  val moves = (for(g <- glasses) yield Empty(g)) ++ (for(g <- glasses) yield Fill(g)) ++ (for(from <- glasses; to <- glasses; if(from != to)) yield Pour(from, to))
   
  //paths
  class Path(history: List[Move], val endState: State){ // define a val to make endState accessible for outside
//    def endState: State = history.foldRight(initialState)(_ change _ )
    def extend(move: Move) = new Path(move::history, move.change(endState))
    override def toString: String = history.reverse mkString(" ") + "-->" + endState
  }
  
  def initialPath = new Path(Nil, initialState)
  
  def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] ={
    if(paths.isEmpty) Stream.empty
    else{ // generate all new path based on the paths
      val more = for{
        path <- paths
        next <- moves.map(path.extend)
        if !(explored contains next.endState)//
      } yield next
      paths #:: from(more, explored ++ (more map (_.endState)))
    }
  }
   
  
  val pathSets = from(Set(initialPath), Set(initialState))
  
  def solutions(target: Int): Stream[Path] =
    for{
      pathSet <- pathSets
      path <- pathSet
      if(path.endState contains target)
    } yield path
}
