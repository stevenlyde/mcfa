package org.ucombinator

trait Sharp {
  def wt (that : Sharp) : Boolean ;

  def resetChangeLog : Sharp ;
  def changeLog : DeltaSharp ;
}

abstract class DeltaSharp {
  def isEmpty : Boolean ;

  def apply(sharp : Sharp) : Sharp ;
}

case class StoreSharp(val store : Store) extends Sharp {

  def resetChangeLog = {
    if (store.isInstanceOf[SentinelStore])
      new StoreSharp(new SentinelStore(store.asInstanceOf[SentinelStore].store))
    else
      new StoreSharp(new SentinelStore(store))
  }
  
  def changeLog = {
    if (store.isInstanceOf[SentinelStore])
      StoreUpdateDeltaSharp(store.asInstanceOf[SentinelStore].changeLog)
    else
      throw new Exception()
  }

  def wt (that : Sharp) : Boolean = that match {
    case StoreSharp(thatStore) => store wt thatStore
    case _ => throw new Exception("Can't compare sharps!")
  }
}

case class StoreUpdateDeltaSharp (val changeLog : List[StoreUpdate]) extends DeltaSharp {
  def isEmpty = changeLog.isEmpty

  def apply (sharp : Sharp) : Sharp = 
    changeLog.foldLeft (sharp) ((sharp,update) => update(sharp))
}

/*
case object NullDeltaSharp extends DeltaSharp {
  override isEmpty = true
  
  def apply(sharp : Sharp) : Sharp = sharp
}
*/
