package org.ucombinator

class ArrayDisjointSet(id: Array[Int], sz: Array[Int]) {

  def this(n: Int) = {
    this(ArrayDisjointSet.initIdArray(n), new Array(n))
  }

  def find(p: Int): Int = {
    var i = p
    while (i != id(i)) {
      id(i) = id(id(i))
      i = id(i)
    }
    i
  }

  def union(p: Int, q: Int): Int = {
    val i = find(p)
    val j = find(q)

    if (sz(i) < sz(j)) {
      id(i) = j
      sz(j) += sz(i)
      j
    }
    else {
      id(j) = i
      sz(i) += sz(j)
      i
    }
  }

  def toSets(): Iterable[Set[Int]] = {
    (0 until id.length).foldLeft(Map[Int, Set[Int]]())((map, p) => {
      val i = find(p)
      map + (i -> (map.getOrElse(i, Set[Int]()) + p))
    }).values
  }

}

object ArrayDisjointSet {

  def initIdArray(n: Int) = {
    val id = new Array[Int](n)
    (0 until n).foreach(i => id(i) = i)
    id
  }

}
