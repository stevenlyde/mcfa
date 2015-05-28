package org.ucombinator

import java.util.IdentityHashMap

import scala.collection.immutable.TreeMap
import scala.collection.mutable.Queue

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

class EnvironmentSizePriorityAssignment extends PriorityAssignment {

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => {
      val priority = state match {
        case State(CFlat(_, MapBEnv(map), _), _) => map.size
        case _ => 0 // we don't know the size of a flat environment
      }
      OrderedState(state, priority)
    })
  }

}

class EnvironmentFlowSetPriorityAssignment extends PriorityAssignment {

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    val StoreSharp(store) = globalSharp
    states.map(state => {
      val priority = state match {
        case State(CFlat(_, MapBEnv(map), _), _) =>
          map.foldLeft(0)((s, entry) => s + store(entry._2).size)

          map.size
        case _ => 0 // we don't know the addresses of a flat environment
      }
      OrderedState(state, priority)
    })
  }

}

class BranchingFactorPriorityAssignment extends PriorityAssignment {

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    val StoreSharp(store) = globalSharp
    states.map(state => {
      val priority = state match {
        case State(CFlat(exp@App(f, args), env, t), _) =>
          f match {
            case Ref(name) => store(env(name)).size
            case _ => 0
          }
        case _ => 0
      }
      OrderedState(state, priority)
    })
  }

}

class ArgumentFlowSetPriorityAssignment extends PriorityAssignment {

  def flowSetSize(exp : Exp, env : BEnv, store : Store): Int = exp match {
    case Ref(name) => store(env(name)).size
    case _ => 1
  }

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    val StoreSharp(store) = globalSharp
    states.map(state => {
      val priority = state match {
        case State(CFlat(exp@App(f, args), env, t), _) =>
          args.positionals.foldLeft(0)((sum, arg) => flowSetSize(arg.exp, env, store)) +
            args.keywords.foldLeft(0)((sum, arg) => flowSetSize(arg.exp, env, store))
        case _ => 0
      }
      OrderedState(state, priority)
    })

  }

}

class ExpressionFrequencyPriorityAssignment extends PriorityAssignment {

  val m: IdentityHashMap[Exp, Int] = new IdentityHashMap()

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    val StoreSharp(store) = globalSharp
    states.map(state => {
      val priority = state match {
        case State(CFlat(exp, _, _), _) =>
          val oldCount = m.get(exp)
          val newCount = if (oldCount == null) 1 else oldCount + 1
          m.put(exp, newCount)
      }
      OrderedState(state, Int.MaxValue - priority)
    })
  }

}

class TimeStampValuePriorityAssignment extends PriorityAssignment {

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    val StoreSharp(store) = globalSharp
    states.map(state => {
      val priority = state match {
        case State(CFlat(_, _, KTime(last)), _) =>
          last.sum
      }
      OrderedState(state, priority)
    })
  }

}

class TimeStampFrequencyPriorityAssignment extends PriorityAssignment {

  var m: Map[Time, Int] = new TreeMap[Time, Int]()

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    val StoreSharp(store) = globalSharp
    states.map(state => {
      val priority = state match {
        case State(CFlat(_, _, t), _) =>
          var count = m.getOrElse(t, 0) + 1
          m = m + (t -> count)
          count
      }
      OrderedState(state, priority)
    })
  }

}

class DepthFirstLabelPriorityAssignment extends PriorityAssignment {

  override def initialize(s: State): Unit = {
    val State(CFlat(exp, _, _), _) = s
    fold(exp)
  }

  private def fold(e: Exp): Unit = {
    e.label
    e match {
      case App(f, args) =>
        fold(f)
        args.positionals.foreach(_.exp)
        args.keywords.foreach(_.exp)

      case Lambda(formals, ExpBody(body)) =>
        fold(body)

      case Seq(SetBang(name, value), call) =>
        fold(value)
        fold(call)

      case If(condition, ifTrue, ifFalse) =>
        fold(condition)
        fold(ifTrue)
        fold(ifFalse)

      case Ref(name) =>
      case Lit(value) =>
      case Void() =>
      case Undefined() =>
    }
  }

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => {
      val priority = state match {
        case State(CFlat(exp, _, _), _) => exp.label
      }
      OrderedState(state, priority)
    })
  }

}


class BreadthFirstLabelPriorityAssignment extends PriorityAssignment {

  override def initialize(s: State): Unit = {
    val State(CFlat(exp, _, _), _) = s
    fold(exp)
  }

  private def fold(e: Exp): Unit = {
    val q = new Queue[Exp]()
    q.enqueue(e)

    while (!q.isEmpty) {
      val current = q.dequeue()
      current.label
      current match {
        case App(f, args) =>
          q.enqueue(f)
          args.positionals.foreach(arg => q.enqueue(arg.exp))
          args.keywords.foreach(arg => q.enqueue(arg.exp))

        case Lambda(formals, ExpBody(body)) =>
          q.enqueue(body)

        case Seq(SetBang(name, value), call) =>
          q.enqueue(value)
          q.enqueue(call)

        case If(condition, ifTrue, ifFalse) =>
          q.enqueue(condition)
          q.enqueue(ifTrue)
          q.enqueue(ifFalse)

        case Ref(name) =>
        case Lit(value) =>
        case Void() =>
        case Undefined() =>
      }
    }
  }

  def prioritize(states: List[State], globalSharp: Sharp): List[OrderedState] = {
    states.map(state => {
      val priority = state match {
        case State(CFlat(exp, _, _), _) => exp.label
      }
      OrderedState(state, Int.MaxValue - priority)
    })
  }

}