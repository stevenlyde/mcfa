package org.ucombinator

import scala.collection.mutable.Map

class DisjointSet[A](id: Map[A, A], sz: Map[A, Int]) {

  def this(ps: Iterable[A]) = {
    this(ps.foldLeft(Map[A, A]())((m, p) => m.updated(p, p)), ps.foldLeft(Map[A, Int]())((m, p) => m.updated(p, 0)))
  }

  def find(p: A): A = {
    var i = p
    while (i != id(i)) {
      id(i) = id(id(i))
      i = id(i)
    }
    i
  }

  def union(p: A, q: A): A = {
    val i = find(p)
    val j = find(q)

    if (sz(i) < sz(j)) {
      id(i) = j
      sz(j) += sz(i)
      j
    }
    else {
      id(j) = i
      sz(i) = +sz(j)
      i
    }
  }

  def toSets(): Iterable[Set[A]] = {
    id.keys.foldLeft(Map[A, Set[A]]())((map, p) => {
      val i = find(p)
      map + (i -> (map.getOrElse(i, Set[A]()) + p))
    }).values
  }

}
