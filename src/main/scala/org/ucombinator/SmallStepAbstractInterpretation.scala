package org.ucombinator

import collection.mutable.PriorityQueue

trait SmallStepAbstractInterpretation {
  def initialState: State;
  def next(state: State): List[State];
  def dependencies(flat: Flat): List[Addr]

  var printStates = false

  var count = 0
  var globalSharp: Sharp = null

  def runWithGlobalSharp() {
    val deps = scala.collection.mutable.HashMap[Addr, Set[Flat]]()
    val seen = scala.collection.mutable.Set[Flat]()

    val init = initialState
    var todo = List(init.flat)

    seen.add(init.flat)
    for (addr <- dependencies(init.flat)) deps(addr) = deps.getOrElse(addr, Set()) + init.flat

    globalSharp = init.sharp
    var timeout = -1

    while (!todo.isEmpty && timeout != 0) {
      var flat = todo.head
      todo = todo.tail

      if (timeout > 0)
        timeout -= 1

      count += 1

      // Install the global sharp:
      var newState = State(flat, globalSharp)

      // Reset the changeLog:
      newState = (newState.sharp = newState.sharp.resetChangeLog)

      if (this.printStates)
        print("State: " + newState)

      // Explore successors:
      val succs = next(newState)

      // Mark the flat as being seen
      seen.add(newState.flat)

      // Check each successor for change:
      for (State(flat, sharp) <- succs) {

        if (!seen.contains(flat)) {
          todo = flat :: todo
          for (addr <- dependencies(flat)) deps(addr) = deps.getOrElse(addr, Set()) + flat
        }

        var delta = sharp.changeLog
        if (!delta.isEmpty) {
          // Something changed!

          // Apply sharp changes to the global store:
          globalSharp = delta(globalSharp)

          // Add the dependencies to the work list
          for {
            addr <- delta.dependencies()
            dep <- deps.getOrElse(addr, Set())
          } todo = dep :: todo

        }
      }
    }

    if (timeout == 0)
      println("Timeout reached")

    println("States explored: " + count)
  }
}
