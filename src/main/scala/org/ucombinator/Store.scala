package org.ucombinator

import scala.collection.immutable.{SortedSet, TreeSet, SortedMap, TreeMap}

trait Store {
  def apply(addr : Addr) : D = (this get addr) match {
    case Some(d) => d
    case None => throw new Exception("Could not find " + addr)
  }
  def getOrElse(addr : Addr, default : D) : D = (this get addr) match {
    case Some(d) => d
    case None => default
  }
  def get(addr : Addr) : Option[D] ;


  def wt (that : Store) : Boolean ;

  /**
   Weak update.
   */
  def + (addr : Addr, d : D) : Store ;

  /**
   Strong update if safe; weak update otherwise.
   */
  def update (addr : Addr, d : D) : Store ;

  def toList : List[(Addr,D)] ;

  def find(a : Addr): Addr ;

  def union (a1 : Addr, a2 : Addr) : Store ;
}

object Store {
  
  // TODO: The code below produces raw stats for comparing analyses.
  // The code will be scrapped and rewritten to produce useful
  // information after the PLDI 2010 deadline.

  type FlowMap = SortedMap[SName,SortedSet[Int]]

  def condense (store : Store) : SortedMap[SName,SortedSet[Int]] = {
    var map : FlowMap = TreeMap[SName,SortedSet[Int]]()

    
    val set : SortedSet[Int] = TreeSet[Int]()

    for ((addr,d) <- store.toList) {
      map =
        addr match {
          case FlatBind(name,_) => {
            (map get name) match {
              case Some(flowSet) => map + (name -> (flowSet ++ (d.toList map (_.toSourceLabel))))
              case None => map + (name -> (set ++ (d.toList map (_.toSourceLabel))))
            }
          }
          case MapBind(name,_) => {
            (map get name) match {
              case Some(flowSet) => map + (name -> (flowSet ++ (d.toList map (_.toSourceLabel))))
              case None => map + (name -> (set ++ (d.toList map (_.toSourceLabel))))
            }
          }
          case _ =>
            map
        }
    }

    map
  }

  def countInlinable (flowMap : FlowMap) : Int = {
    var inlinable = 0 

    for ((name,flows) <- flowMap) {
      if (flows.size == 1 && !(flows exists (n => n < 0))) {
        inlinable += 1
      }
    }
    
    inlinable
  }

  abstract class Score
  case object LeftWins extends Score
  case object RightWins extends Score
  case object Tie extends Score
  case object NeitherWins extends Score

  def crossAnalyze (flowMap1 : FlowMap, flowMap2 : FlowMap) {
    var scoreMap : SortedMap[SName,Score] = TreeMap[SName,Score]()
   
    for ((name,flows) <- flowMap1) {
      (flowMap1 get name,flowMap2 get name) match {
        case (Some(flows1),Some(flows2)) => {

          println("pflows["+name+"]: " +flows1)
          println("mflows["+name+"]: " +flows2)

          val lw = flows1 subsetOf flows2
          var rw = flows2 subsetOf flows1

          println("lw: " + lw)
          println("rw: " + rw)

          if (lw && rw)
            scoreMap = (scoreMap + (name -> Tie))
          else if (lw)
            scoreMap = (scoreMap + (name -> LeftWins))
          else if (rw)
            scoreMap = (scoreMap + (name -> RightWins))
          else
            scoreMap = (scoreMap + (name -> NeitherWins))
        }
        case (Some(_), None) => {
          scoreMap = (scoreMap + (name -> RightWins))
        }
        case (None,Some(_)) =>
          scoreMap = (scoreMap + (name -> LeftWins))
      }
    }


    for ((name,flows) <- flowMap2) {
      (flowMap1 get name,flowMap2 get name) match {
        case (Some(flows1),Some(flows2)) => {
          val lw = flows1 subsetOf flows2
          var rw = flows2 subsetOf flows1
          if (lw && rw)
            scoreMap = (scoreMap + (name -> Tie))
          else if (lw)
            scoreMap = (scoreMap + (name -> LeftWins))
          else if (rw)
            scoreMap = (scoreMap + (name -> RightWins))
          else
            scoreMap = (scoreMap + (name -> NeitherWins))
        }
        case (Some(_), None) => {
          scoreMap = (scoreMap + (name -> RightWins))
        }
        case (None,Some(_)) =>
          scoreMap = (scoreMap + (name -> LeftWins))
      }
    }

    var LW = 0
    var RW = 0
    var TIE = 0 
    var NEITHER = 0

    for ((name,score) <- scoreMap) {
      if (score == LeftWins)
        LW += 1
      else if (score == RightWins)
        RW += 1
      else if (score == Tie)
        TIE += 1
      else
        NEITHER += 1
    }
 
    println("LW:      " + LW + "\n ", 
            "RW:      " + RW + "\n ", 
            "TIE:     " + TIE + "\n ", 
            "NEITHER: " + NEITHER + "\n ")
  }
}
