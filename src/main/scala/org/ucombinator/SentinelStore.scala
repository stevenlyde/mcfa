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
        new SentinelStore(StoreUpdate(false,addr,d) :: changeLog, store + (addr,d))
      }
    }
  }

  def update (addr : Addr, d : D) : SentinelStore = {
    (store get addr) match {
      case Some(d2) if (d wt d2) => this
      case _ => {
        new SentinelStore(StoreUpdate(true,addr,d) :: changeLog, store(addr) = d)
      }
    }
  }

  override def toString = store.toString

  def toList = store.toList
}

case class StoreUpdate (val isStrong : Boolean, val addr : Addr, val d : D) {
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

