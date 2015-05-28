package org.ucombinator

import scala.collection.immutable.{SortedSet, TreeSet, SortedMap, TreeMap}

/* Small-step abstract interpretation */


/**
 A state in a small-step abstract interpretation.

 The <code>flat</code> component of a state has a flat partial order;
 the <code>sharp</code> component of a state has a true partial order.
 */
case class State(_flat : Flat, _sharp : Sharp) {
  def flat = _flat

  def sharp_= (_sharp : Sharp) : State = 
    State(_flat,_sharp)

  def sharp = _sharp
}

trait Flat extends Ordered[Flat] {
  override def equals (that : Any) = that match {
    case that : Flat => (this compare that) == 0
    case _ => false
  }
}

case object StuckFlat extends Flat {
  def compare (that : Flat) = 
    that match {
      case StuckFlat => 0
      case _ => -1
    }
}














/* Flow analyses */

trait Addr extends Ordered[Addr] {

  def localCompare (that : Addr) : Int ;

  def compare (that : Addr) : Int = {
    val thisClass = this.getClass() 
    val thatClass = that.getClass()
    val cmp = thisClass.getName() compare thatClass.getName()
    if (cmp != 0)
      cmp
    else
      localCompare(that)
  }

}

trait ObjectLocation extends Value {
  def isProcedure = false
  def isObjectLocation = true
  def objectType : SName ;
}

case class ConsLocation(time : Time) extends ObjectLocation {
  def localCompare (that : Value) : Int = that match {
    case ConsLocation(thatTime) => time compare thatTime
  }

  val objectType = SName.from("cons")

  def toSourceLabel : Int = time match {
    case KTime(hd :: tl) => -hd + -10
    case _ => -1
  }
}

case class FieldAddr(baseAddr : ObjectLocation, field : SName) extends Addr {
  def localCompare (that : Addr) : Int = {
    that match {
      case FieldAddr(ba2,field2) => ComparisonUtilities.compare2 (baseAddr.asInstanceOf[Value],field) (ba2.asInstanceOf[Value],field2)
    }
  }
}



trait BEnv extends Ordered[BEnv] {
  def apply(name : SName) : Addr ;
  override def equals (that : Any) = that match {
    case thatBEnv : BEnv => (this compare thatBEnv) == 0
    case _ => false
  }
  def update(name : SName, addr : Addr) : BEnv ;
  def | (names : Iterable[SName]) : BEnv ;
}

trait Time extends Ordered[Time] {
  def succ (k : Int, call : Int) : Time ;
}



trait Kont

trait Value extends Ordered[Value] {
  def isProcedure : Boolean ;
  def isObjectLocation : Boolean ;

  protected def localCompare(that : Value) : Int ;

  def compare (that : Value) : Int = {
    val thisClassName = this.getClass().getName()
    val thatClassName = that.getClass().getName()
    val cmp = thisClassName compare thatClassName
    if (cmp != 0)
      cmp
    else
      this.localCompare(that)
  }

  override def equals (that : Any) : Boolean = that match {
    case that : Value => (this compare that) == 0
    case _ => false
  }

  def toSourceLabel : Int ;
}

case class BooleanValue(val value : Boolean) extends Value {
  override def toString = if (value) { "#t" } else { "#f" }

  def localCompare (that : Value) : Int = that match {
    case BooleanValue(value2) => value compare value2
  }
  
  def isProcedure = false
  def isObjectLocation = false

  def toSourceLabel : Int = -2
}


case class PrimValue(val name : String) extends Value {
  def localCompare (that : Value) : Int = that match {
    case PrimValue(name2 : String) => name compare name2
  }

  def isProcedure = true
  def isObjectLocation = false

  def toSourceLabel : Int = -3
}


case class Clo(val lam : Lambda, val bEnv : BEnv) extends Value {
  def localCompare (that : Value) : Int = that match {
    case Clo(lam2,bEnv2) => ComparisonUtilities.compare2 (lam.asInstanceOf[Exp],bEnv) (lam2.asInstanceOf[Exp],bEnv2)
  }

  def isProcedure = true 
  def isObjectLocation = false

  def toSourceLabel : Int = lam.label
}

trait D {
  def + (value : Value) : D ;
  def join (that : D) : D ;
  def wt (that : D) : Boolean ;
  def toList : List[Value] ;
  def size : Int
}






/* Generics components */









case class SortedSetD (set : SortedSet[Value]) extends D {
  def this () = this (TreeSet())

  def + (value : Value) : D = SortedSetD(set + value)
  
  def join (that : D) = SortedSetD(set ++ that.toList)

  def wt (that : D) : Boolean = {
    that match {
      case SortedSetD(set2) => set subsetOf set2
    }
  }

  def toList = set.toList

  def size = set.size

  override def toString = "{" + (set mkString ",") + "}"
}







case class KTime(val last : List[Int]) extends Time {
  def compare (that : Time) =  that match {
    case KTime(last2) => ComparisonUtilities.compareLists (last,last2)
  }
  def succ (k : Int, call : Int) : KTime = KTime((call :: last) take k)
}


case object UniTime extends Time {
  def compare (that : Time) = 
    if (this eq that)
      0
    else
      -1

  def succ (k : Int, call : Int) : Time = this
}


case class PrimAddr(val name : SName) extends Addr {
  def localCompare (that : Addr) : Int = that match {
    case PrimAddr(thatName) => this.name compare thatName
  }
}






/* mCFA */


case class FlatBind (name : SName, bEnv : BEnv) extends Addr {
  def localCompare (that : Addr) : Int = {
    that match {
      case FlatBind(thatName, thatBEnv : BEnv) =>
        ComparisonUtilities.compare2 (name,bEnv) (thatName,thatBEnv)
    }
  }
}

case class FlatBEnv (val labels : List[Int]) extends BEnv {
  def apply (name : SName) = FlatBind(name,this)
  def succ (m : Int, l : Int) = FlatBEnv((l::labels) take m)

  def compare (that : BEnv) = that match {
    case that : FlatBEnv => ComparisonUtilities.compareLists (this.labels, that.labels)
  }
  
  def update (name : SName, addr : Addr) : BEnv = throw new Exception("Cannot extend flat binding environments.")

  def | (names : Iterable[SName]) : BEnv = this
}


case class MapBind (val name : SName, val time : Time) extends Addr {
  def localCompare (that : Addr) : Int = that match {
    case MapBind(thatName,thatTime) => ComparisonUtilities.compare2 (this.name,this.time) (thatName,thatTime)
  }
}

case class MapBEnv (val map : SortedMap[SName,Addr]) extends BEnv {
  def apply (name : SName) = map(name)
  def succ (m : Int, l : Int) = throw new Exception("Not appropriate in this context.")
  
  def compare (that : BEnv) = that match {
    case MapBEnv(thatMap) => ComparisonUtilities.compareLists (this.map.toList,thatMap.toList)
  }
  
  def update (name : SName, addr : Addr) : BEnv = 
    MapBEnv(map + (name -> addr))

  def | (names : Iterable[SName]) : BEnv = MapBEnv(TreeMap[SName,Addr]() ++ (names map (n => (n,map(n)))))
    
}


case class CFlat(val exp : Exp, val bEnv : BEnv, val t : Time) extends Flat {
  def compare (that : Flat) = that match {
    case CFlat(exp2,bEnv2,t2) => ComparisonUtilities.compare3 (exp,bEnv,t) (exp2,bEnv2,t2)
  }
}

