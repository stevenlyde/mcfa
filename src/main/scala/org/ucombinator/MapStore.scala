package org.ucombinator

import scala.collection.immutable.{SortedMap, TreeMap}

class MapStore(val map : SortedMap[Addr,D]) extends Store {
  def this () = this(TreeMap())

  def get(addr : Addr) : Option[D] = map get addr

  def wt (that : Store) = that match {
    case _ => throw new Exception("Unknown store type")
  }

  /**
   Weak update.
   */
  def + (addr : Addr, d : D) : MapStore = {
    map get addr match {
      case Some(existingD) => new MapStore(map + (addr -> (d join existingD)))
      case None => new MapStore(map + (addr -> d))
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
}
