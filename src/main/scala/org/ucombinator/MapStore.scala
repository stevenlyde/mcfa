package org.ucombinator

import scala.collection.immutable.{SortedMap, TreeMap, HashMap}

class MapStore(map : SortedMap[Addr,D], parent : Map[Addr, Addr], rank : Map[Addr, Int]) extends Store {
  def this () = this(TreeMap(), HashMap(), HashMap())

  def get(addr : Addr) : Option[D] = map get find(addr)

  def wt (that : Store) = that match {
    case _ => throw new Exception("Unknown store type")
  }

  /**
   Weak update.
   */
  def + (addr : Addr, d : D) : MapStore = {
    val (p, newParent) = find(addr, parent)
    map get p match {
      case Some(existingD) =>
        new MapStore(map + (p -> (d join existingD)), newParent, rank)
      case None =>
        new MapStore(map + (p -> d), newParent, rank)
    }
  }

  /**
   A simple store does not contain enough information to determine whether a strong update is safe or not, so
   this operation always performs a weak update.
   */
  def update (addr : Addr, d : D) : Store = {
    // We don't have enough information to do a strong update, so we fall back to a weak update.
    this + (addr,d)
  }

  override def toString = "\n " +  (map mkString "\n ") 

  def toList : List[(Addr,D)] = map.toList

  def find(a: Addr): Addr = {
    val p = parent.getOrElse(a, a)
    if (a == p) p else find(p)
  }

  def find(a: Addr, parent: Map[Addr, Addr]): (Addr, Map[Addr, Addr]) = {
    val p = parent.getOrElse(a, a)
    if (a == p) (p, parent) else {
      val (newP, newParent) = find(p, parent)
      (newP, newParent + (a -> newP))
    }
  }

  def union(a1 : Addr, a2 : Addr) = {
    val (p1, parent1) = find(a1, parent)
    val (p2, parent2) = find(a2, parent1)
    if (p1 == p2) {
      this
    }
    else {
      val bottom = new SortedSetD()
      val d = map.getOrElse(p1, bottom) join map.getOrElse(p2, bottom)

      val rank1 = rank.getOrElse(p1, 0)
      val rank2 = rank.getOrElse(p2, 0)

      val (newP, oldP, newParent) =
        if (rank1 < rank2)
          (p2, p1, parent2 + (p1 -> p2))
        else
          (p1, p2, parent2 + (p2 -> p1))

      val newRank = if (rank1 == rank2) rank + (p1 -> (rank1 + 1)) else rank

      new MapStore(map + (newP -> d) - oldP, newParent, newRank)
    }
  }
}
