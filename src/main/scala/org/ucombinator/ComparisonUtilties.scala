package org.ucombinator

object ComparisonUtilities {

  def compare2[A <: Ordered[A], B <: Ordered[B]] (a1 : A, b1 : B) (a2 : A, b2 : B) : Int = {
    val cmpA = a1 compare a2
    if (cmpA != 0)
      cmpA
    else
      b1 compare b2
  }

  def compare3[A <: Ordered[A], B <: Ordered[B], C <: Ordered[C]] (a1 : A, b1 : B , c1 : C) (a2 : A, b2 : B, c2 : C) : Int = {
    val cmpA = a1 compare a2
    if (cmpA != 0)
      cmpA
    else {
      val cmpB = b1 compare b2
      if (cmpB != 0)
        cmpB
      else
        c1 compare c2
    }
  }


  def compareLists[A <% Ordered[A]] (l1 : List[A], l2 : List[A]) : Int = (l1,l2) match {
    case (hdA :: tlA, hdB :: tlB) => {
      val cmpHD = hdA compare hdB
      if (cmpHD != 0)
        cmpHD
      else
        compareLists(tlA,tlB)
    }
    case (List(),List()) => 0 
    case (hd :: tl,List()) => 1    
    case (List(),hd :: tl) => -1
  }
}