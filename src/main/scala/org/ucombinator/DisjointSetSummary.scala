package org.ucombinator

import scala.collection.immutable.{SortedSet, SortedMap, TreeSet, TreeMap}

object DisjointSetSummary {

  type FlowMap = SortedMap[SName, SortedSet[Int]]

  def condense(ps: DisjointSet[Int], vs: Map[SName, Int], ts: LabelTypes): SortedMap[SName, SortedSet[Int]] = {
    val sets = ps.toSets().foldLeft(Map[Int, SortedSet[Int]]())((sets, set) => {
      val ss = TreeSet[Int]() ++ set.filter(ts.isLambda(_))
      set.foldLeft(sets)((sets, label) => sets + (label -> ss))
    })

    var map: FlowMap = TreeMap[SName, SortedSet[Int]]()
    for ((v, l) <- vs) {
      map = map + (v -> sets(l))
    }
    map
  }

}
