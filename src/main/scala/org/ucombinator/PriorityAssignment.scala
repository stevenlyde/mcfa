package org.ucombinator

case class OrderedState(state: State, priority: Int) extends Ordered[OrderedState] {
  override def compare(that: OrderedState): Int = priority - that.priority
}

trait PriorityAssignment {
  
  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState]

}

class DepthFirstSearchPriorityAssignment extends PriorityAssignment {

  var priority = 0

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.reverse.map(state => {
      priority += 1
      OrderedState(state, priority)
    })
  }

}

class BreadthFirstSearchPriorityAssignment extends PriorityAssignment {

  var priority: Int = Int.MaxValue

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => {
      priority -= 1
      OrderedState(state, priority)
    })
  }

}

class ConstantPriorityAssignment extends PriorityAssignment {

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => {
      OrderedState(state, 0)
    })
  }

}
