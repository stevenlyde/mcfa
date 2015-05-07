package org.ucombinator

class SentinelStore(val changeLog : List[StoreUpdate], val store : Store) extends Store {

  def this (store : Store) = this(List(), store)

  def get (addr : Addr) = store get addr
  def wt (that : Store) = that match {
    case thatStore : SentinelStore => store wt thatStore.store
    case _ => store wt that
  }

  def resetLog() = 
    new SentinelStore(List(), store)

  def + (addr : Addr, d : D) : SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(BindingUpdate(false,addr,d) :: changeLog, store + (addr,d))
      }
    }
  }

  def update (addr : Addr, d : D) : SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(BindingUpdate(true,addr,d) :: changeLog, store(addr) = d)
      }
    }
  }

  override def toString = store.toString

  def toList = store.toList

  def find(a1 : Addr) = store find a1

  def union(a1 : Addr, a2 : Addr) = {
    if (store.find(a1) != store.find(a2))
      new SentinelStore(UnionUpdate(a1, a2) :: changeLog, store union (a1, a2))
    else
      this
  }
}

trait StoreUpdate {
  def apply(sharp : Sharp) : Sharp
}

case class BindingUpdate (val isStrong : Boolean, val addr : Addr, val d : D) extends StoreUpdate {
  def apply(sharp : Sharp) : Sharp = {
    sharp match {
      case StoreSharp(store) => 
        if (isStrong)
          new StoreSharp (store(addr) = d)
        else
          new StoreSharp (store + (addr,d))
    }
  }
}

case class UnionUpdate(a1: Addr, a2: Addr) extends StoreUpdate {
  def apply(sharp : Sharp) : Sharp = sharp match {
    case StoreSharp(store) =>
      new StoreSharp (store union (a1, a2))
  }
}
