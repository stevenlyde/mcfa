package org.ucombinator

import java.util.IdentityHashMap

case class OrderedState(state: State, priority: Int) extends Ordered[OrderedState] {
  override def compare(that: OrderedState): Int = priority - that.priority
}

trait PriorityAssignment {

  def initialize(s: State) {}

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

class ExpressionTypePriorityAssignment extends PriorityAssignment {

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => state match {
      case State(CFlat(exp, _, _), _) =>
        val priority = exp match {
          case App(_, _) => 3
          case If(_, _, _) => 2
          case Seq(_) => 1
          case _ => 0
        }
        OrderedState(state, priority)
    })
  }

}

class ExpressionSizePriorityAssignment extends PriorityAssignment {

  val m: IdentityHashMap[Exp, Int] = new IdentityHashMap()

  override def initialize(s: State): Unit = {
    val State(CFlat(exp, _, _), _) = s
    val sizer = new ExpressionSizer
    sizer.populate(exp, m)
  }

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => state match {
      case State(CFlat(exp, _, _), _) =>
        OrderedState(state, m.get(exp))
    })
  }

}

class ExpressionSizer() {

  private def fold(e: Exp, m: IdentityHashMap[Exp, Int]): Int = {
    e match {
      case App(f, args) =>
        val s1 = fold(f, m)
        val s2 = args.positionals.foldLeft(0)((s, arg) => s + fold(arg.exp, m))
        val s3 = args.keywords.foldLeft(0)((s, arg) => s + fold(arg.exp, m))
        m.put(e, 1 + s1 + s2 + s3)

      case Lambda(formals, ExpBody(body)) =>
        val s1 = fold(body, m)
        m.put(e, 1 + s1)

      case Seq(SetBang(name, value), call) =>
        val s1 = fold(value, m)
        val s2 = fold(call, m)
        m.put(e, 2 + s1 + s2)

      case If(condition, ifTrue, ifFalse) =>
        val s1 = fold(condition, m)
        val s2 = fold(ifTrue, m)
        val s3 = fold(ifFalse, m)
        m.put(e, 1 + s1 + s2 + s3)

      case Ref(name) => 1
      case Lit(value) => 1
      case Void() => 1
      case Undefined() => 1
    }
  }

  def populate(e: Exp, m: IdentityHashMap[Exp, Int]) {
    fold(e, m)
  }

}
