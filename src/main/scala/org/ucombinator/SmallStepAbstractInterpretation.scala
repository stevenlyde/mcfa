package org.ucombinator

import collection.mutable.PriorityQueue

trait SmallStepAbstractInterpretation {
  def initialState: State;
  def next(state: State): List[State];

  var printStates = false

  var count = 0
  var globalSharp: Sharp = null

  def runWithGlobalSharp() {
    // seen records the last generation store seen with this flat.
    val seen = scala.collection.mutable.HashMap[Flat, Long]()
    var currentGeneration: Long = 1

    val init = initialState
    val todo = new PriorityQueue[OrderedState]()
    val assignment: PriorityAssignment = new BranchingFactorPriorityAssignment
    assignment.initialize(init)
    todo ++= assignment.prioritize(List(init), init.sharp)

    globalSharp = init.sharp
    var timeout = -1

    while (!todo.isEmpty && timeout != 0) {
      var newState = todo.dequeue().state

      val flat = newState.flat
      val sharp = newState.sharp

      val lastSeenGeneration: Long = seen.getOrElse(flat, 0)

      if (currentGeneration <= lastSeenGeneration) {
        // println("Current generation: " + currentGeneration) // DEBUG
        // println("Last generation: " + lastSeenGeneration) // DEBUG
        // println("Not a new state: " + newState) // DEBUG
      }

      if (currentGeneration > lastSeenGeneration) {
        // globalSharp changed since we last saw this flat.
        if (timeout > 0)
          timeout -= 1

        count += 1

        if (this.printStates)
          print("State: " + newState)

        // Install the global sharp:
        newState = (newState.sharp = globalSharp)

        //println("State:\n" + newState + "\n")

        // Reset the changeLog:
        newState = (newState.sharp = newState.sharp.resetChangeLog)

        // Explore successors:
        val succs = next(newState)

        // Mark this state as being seen with the current generation.
        seen(newState.flat) = currentGeneration

        // Check each successor for change:
        for (succ <- succs) {
          var sharp = succ.sharp
          var delta = sharp.changeLog
          if (!delta.isEmpty) {
            // Something changed!

            // Bump up the current sharp generation:
            currentGeneration += 1

            // Apply sharp changes to the global store:
            globalSharp = delta(globalSharp)
          }
        }
        todo ++= assignment.prioritize(succs, globalSharp)
      }
    }

    if (timeout == 0)
      println("Timeout reached")

    println("States explored: " + count)
  }
}
